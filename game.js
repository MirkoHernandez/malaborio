window.addEventListener("load", async () => {
  try {
    await Scheme.load_main("game.wasm", {}, {
      window: {
        get: () => window,
        innerWidth: () => window.innerWidth,
        innerHeight: () => window.innerHeight,
        requestAnimationFrame: (f) => window.requestAnimationFrame(f),
        setTimeout: (f, delay) => window.setTimeout(f, delay)
      },
      document: {
        get: () => document,
        body: () => document.body,
        fullscreenElement: () => document.fullscreenElement,
        exitFullscreen: () => document.exitFullscreen(),
        getElementById: (id) => document.getElementById(id),
        createTextNode: (text) => document.createTextNode(text),
        createElement: (tag) => document.createElement(tag)
      },
      element: {
        value: (elem) => elem.value,
        addColorStop: (elem,alpha, color) => elem.addColorStop(alpha,color),
        requestFullscreen: (elem) => elem.requestFullscreen(),
        setValue: (elem, value) => elem.value = value,
        width: (elem) => elem.width,
        height: (elem) => elem.height,
        setWidth: (elem, width) => elem.width = width,
        setHeight: (elem, height) => elem.height = height,
        appendChild: (parent, child) => parent.appendChild(child),
        setAttribute: (elem, name, value) => elem.setAttribute(name, value),
        removeAttribute: (elem, name) => elem.removeAttribute(name),
        remove: (elem) => elem.remove(),
        replaceWith: (oldElem, newElem) => oldElem.replaceWith(newElem),
        clone: (elem) => elem.cloneNode()
      },
      event: {
        addEventListener: (target, type, listener) => target.addEventListener(type, listener),
        removeEventListener: (target, type, listener) => target.removeEventListener(type, listener),
        preventDefault: (event) => event.preventDefault(),
        keyboardCode: (event) => event.code
      },
      image: {
        new: (src) => {
          const img = new Image();
          img.src = src;
          return img;
        }
      },
      media: {
        newAudio: (src) => new Audio(src),
        play: (media) => media.play(),
        pause: (media) => media.pause(),
        volume: (media) => media.volume,
        setVolume: (media, volume) => media.volume = volume,
        setLoop: (media, loop) => media.loop = (loop == 1),
        seek: (media, time) => media.currentTime = time
      },
      canvas: {
        getContext: (elem, type) => elem.getContext(type),
        setFillColor: (ctx, color) => ctx.fillStyle = color,
        setFillColorWithGradient: (ctx, color) => ctx.fillStyle = color,
        beginPath: (ctx) => ctx.beginPath(),
        stroke: (ctx) => ctx.stroke(),
        strokeStyle: (ctx,color) => ctx.strokeStyle = color,
        moveTo: (ctx, x,y) => ctx.moveTo(x,y),
        lineTo: (ctx, x,y) => ctx.lineTo(x,y),
        lineWidth: (ctx, w) => ctx.lineWidth = w,
        lineCap: (ctx, w) => ctx.lineCap = w,
        restore: (ctx) => ctx.restore(),
        save: (ctx) => ctx.save(),
        rotate: (ctx,r) => ctx.rotate(r),
        translate: (ctx,x,y) => ctx.translate(x,y),
        createLinearGradient: (ctx,x,y,x2,y2) => ctx.createLinearGradient(x,y,x2,y2),
        setFont: (ctx, font) => ctx.font = font,
        setTextAlign: (ctx, align) => ctx.textAlign = align,
        clearRect: (ctx, x, y, w, h) => ctx.clearRect(x, y, w, h),
        fillRect: (ctx, x, y, w, h) => ctx.fillRect(x, y, w, h),
        fillText: (ctx, text, x, y) => ctx.fillText(text, x, y),
        drawImage: (ctx, image, sx, sy, sw, sh, dx, dy, dw, dh) => ctx.drawImage(image, sx, sy, sw, sh, dx, dy, dw, dh),
        setScale: (ctx, sx, sy) => ctx.scale(sx, sy),
        setTransform: (ctx, a, b, c, d, e, f) => ctx.setTransform(a, b, c, d, e, f),
        setImageSmoothingEnabled: (ctx, enabled) => ctx.imageSmoothingEnabled = (enabled == 1)
      },
      math: {
        random: () => Math.random()
      }
    });
  } catch(e) {
    if(e instanceof WebAssembly.CompileError) {
      document.getElementById("wasm-error").hidden = false;
    }
    throw e;
  }
});
