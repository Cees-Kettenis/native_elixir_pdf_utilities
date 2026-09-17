"""Private Linux openat2 transport. Acceptance decisions stay in FileValidator."""
import ctypes
import errno
import os
import platform
import selectors
import struct
import sys
import threading
import time


def send(payload):
    data = memoryview(struct.pack("!I", len(payload)) + payload)
    try:
        while data:
            data = data[os.write(1, data):]
    except BrokenPipeError:
        os._exit(0)


class OpenHow(ctypes.Structure):
    _fields_ = [("flags", ctypes.c_uint64), ("mode", ctypes.c_uint64), ("resolve", ctypes.c_uint64)]


def open_beneath(directory, path, flags):
    # Linux UAPI invariants: syscall 437 on supported architectures;
    # RESOLVE_BENEATH | RESOLVE_NO_SYMLINKS also forbids magic links.
    how = OpenHow(flags | os.O_CLOEXEC, 0, 0x08 | 0x04)
    libc = ctypes.CDLL(None, use_errno=True)
    libc.syscall.restype = ctypes.c_long
    fd = libc.syscall(ctypes.c_long(437), ctypes.c_int(directory), ctypes.c_char_p(os.fsencode(path)), ctypes.byref(how), ctypes.c_size_t(ctypes.sizeof(how)))
    if fd < 0:
        raise OSError(ctypes.get_errno(), "confined open failed")
    return fd


root, relative, timeout = sys.argv[1:]
if platform.system() != "Linux" or platform.machine() not in ("x86_64", "aarch64"):
    send(b"\x04")
    sys.exit(0)

ready = threading.Event()
allowance = []
finished_r, finished_w = os.pipe()


def read_file():
    opened = []
    try:
        slash = os.open("/", os.O_PATH | os.O_DIRECTORY | os.O_CLOEXEC)
        opened.append(slash)
        root_fd = open_beneath(slash, root.lstrip("/") or ".", os.O_PATH | os.O_DIRECTORY)
        opened.append(root_fd)
        file_fd = open_beneath(root_fd, relative, os.O_RDONLY | os.O_NONBLOCK)
        opened.append(file_fd)
        info = os.fstat(file_fd)
        send(struct.pack("!BIQ", 1, info.st_mode, info.st_size))
        ready.wait()
        remaining = allowance[0] + 1
        chunks = []
        while remaining:
            chunk = os.read(file_fd, min(remaining, 65536))
            if not chunk:
                break
            chunks.append(chunk)
            remaining -= len(chunk)
        result = b"\x00" + b"".join(chunks)
    except OSError as error:
        result = b"\x04" if error.errno == errno.ENOSYS else struct.pack("!BI", 2, error.errno or errno.EIO)
    except Exception:
        result = b"\x05"
    finally:
        for fd in reversed(opened):
            os.close(fd)
    send(result)
    os.write(finished_w, b"1")


threading.Thread(target=read_file, daemon=True).start()
selector = selectors.DefaultSelector()
selector.register(0, selectors.EVENT_READ)
selector.register(finished_r, selectors.EVENT_READ)
deadline = time.monotonic() + int(timeout) / 1000.0
command = bytearray()
while True:
    remaining = deadline - time.monotonic()
    if remaining <= 0:
        # Only the read thread writes packets; concurrent timeout output could
        # interleave a large payload and corrupt its framing.
        os._exit(124)
    for key, _ in selector.select(remaining):
        if key.fd == finished_r:
            os._exit(0)
        part = os.read(0, 12 - len(command))
        if not part:
            # Closing the caller's port terminates even a blocked read thread.
            os._exit(0)
        command.extend(part)
        if len(command) == 12:
            length, maximum = struct.unpack("!IQ", command)
            if length != 8 or maximum == 0 or allowance:
                os._exit(0)
            allowance.append(maximum)
            ready.set()
            command.clear()
