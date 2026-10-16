(() => {
  const targets = [];
  const outside = [];
  const stylesOf = element => {
    const style = getComputedStyle(element);
    return Array.from(style, name => [name, style.getPropertyValue(name)]);
  };
  const visit = (doc, offsetX, offsetY) => {
    for (const element of doc.querySelectorAll('svg, img, object, embed, iframe')) {
      if (element.parentElement?.closest('svg')) continue;
      const tag = element.localName;
      let child;
      try { child = element.contentDocument || element.getSVGDocument?.(); } catch (_) {}
      const svgDocument = child?.documentElement?.namespaceURI === 'http://www.w3.org/2000/svg';
      const source = element.currentSrc || element.src || element.data || '';
      const svgImage = tag === 'img' && (/^data:image\/svg\+xml[,;]/i.test(source) || /\.svg(?:[?#]|$)/i.test(source));
      const style = getComputedStyle(element);
      const rect = element.getBoundingClientRect();
      if (tag === 'svg' || svgDocument || svgImage) {
        if (style.display === 'none' || rect.width === 0 || rect.height === 0) continue;
        if (tag === 'svg' && style.overflow === 'visible') throw new Error('SVG PNG reference requires the root viewport to clip its artwork');
        if (style.transform !== 'none') throw new Error('SVG PNG reference requires an untransformed host viewport');
        const left = parseFloat(style.borderLeftWidth) + parseFloat(style.paddingLeft);
        const top = parseFloat(style.borderTopWidth) + parseFloat(style.paddingTop);
        const width = rect.width - left - parseFloat(style.borderRightWidth) - parseFloat(style.paddingRight);
        const height = rect.height - top - parseFloat(style.borderBottomWidth) - parseFloat(style.paddingBottom);
        // Match the CSS pixel edges used by replaced-image painting. Cropping
        // at fractional layout coordinates would bake in another sampling offset.
        const x = Math.round(offsetX + rect.x + left);
        const y = Math.round(offsetY + rect.y + top);
        const paintedWidth = Math.round(offsetX + rect.x + left + width) - x;
        const paintedHeight = Math.round(offsetY + rect.y + top + height) - y;
        targets.push({element, styles: stylesOf(element), rect: rect.toJSON(), x, y, width: paintedWidth, height: paintedHeight});
      } else if (child && tag === 'iframe') {
        visit(child, offsetX + rect.x + parseFloat(style.borderLeftWidth), offsetY + rect.y + parseFloat(style.borderTopWidth));
      }
    }
    for (const element of doc.querySelectorAll('body *')) {
      if (element.closest('svg') || targets.some(target => target.element === element)) continue;
      outside.push({element, rect: element.getBoundingClientRect().toJSON(), styles: stylesOf(element)});
    }
  };
  visit(document, 0, 0);
  const prepare = index => {
    const target = targets[index];
    target.captureChanges = [];
    for (let element = target.element; element; element = element.parentElement) {
      const properties = element === target.element ? ['background-color', 'background-image'] : ['background-color', 'background-image', 'opacity'];
      for (const name of properties) {
        target.captureChanges.push([element, name, element.style.getPropertyValue(name), element.style.getPropertyPriority(name)]);
        element.style.setProperty(name, name === 'opacity' ? '1' : name === 'background-color' ? 'transparent' : 'none', 'important');
      }
    }
  };
  const restore = index => {
    for (const [element, name, value, priority] of targets[index].captureChanges) {
      if (value) element.style.setProperty(name, value, priority);
      else element.style.removeProperty(name);
    }
  };
  const replace = (index, data) => {
    const target = targets[index];
    const doc = target.element.ownerDocument;
    // Inline SVG styles are document styles. Preserve their source order and
    // activation after removing the SVG subtree, including styles on HTML text.
    if (target.element.localName === 'svg') {
      for (const original of target.element.querySelectorAll('style')) {
        const style = doc.createElement('style');
        for (const attribute of original.attributes) style.setAttribute(attribute.name, attribute.value);
        style.textContent = original.textContent;
        target.element.before(style);
        if (original.sheet?.disabled) style.sheet.disabled = true;
      }
    }
    const image = doc.createElement('img');
    for (const [name, value] of target.styles) image.style.setProperty(name, value, 'important');
    // These SVG effects are already painted into the captured PNG.
    for (const name of ['opacity', 'filter', 'mask', 'clip-path']) image.style.setProperty(name, name === 'opacity' ? '1' : 'none', 'important');
    // The capture already includes the source object's fit and positioning.
    image.style.setProperty('object-fit', 'fill', 'important');
    image.src = 'data:image/png;base64,' + data;
    image.alt = '';
    target.element.replaceWith(image);
    target.replacement = image;
    return image.decode();
  };
  const changed = () => {
    const moved = (element, rect) => ['x', 'y', 'width', 'height'].some(key => Math.round(element.getBoundingClientRect()[key] * 64) !== Math.round(rect[key] * 64));
    return [
      ...outside.filter(({element, rect, styles}) => element.isConnected && (moved(element, rect) || styles.some(([name, value]) => getComputedStyle(element).getPropertyValue(name) !== value))).map(({element}) => element.localName),
      ...targets.filter(target => moved(target.replacement, target.rect)).map(target => 'SVG viewport ' + JSON.stringify({before:target.rect,after:target.replacement.getBoundingClientRect().toJSON()}))
    ];
  };
  globalThis.__nativePdfSvgReference = {prepare, restore, replace, changed};
  return targets.map(({x, y, width, height}, index) => ({index, x, y, width, height}));
})()
