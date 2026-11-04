(async () => {
  if (document.readyState !== 'complete') {
    await new Promise(resolve => addEventListener('load', resolve, {once: true}));
  }
  // Layout selects the fonts actually used by this document, including a child
  // reached through object/embed whose font set differs from the parent.
  document.documentElement?.getBoundingClientRect();
  await document.fonts.ready;
  const images = await Promise.all(Array.from(document.images || [], async image => {
    try { await image.decode(); } catch (_) { /* Broken images retain their fallback. */ }
    return {source: image.currentSrc || image.src, complete: image.complete, width: image.naturalWidth};
  }));
  document.documentElement?.getBoundingClientRect();
  return {url: document.URL, encoding: document.characterSet, ready: document.readyState, fonts: document.fonts.status, images};
})()
