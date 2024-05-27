modules = \
  modules/dom/canvas.scm \
  modules/dom/document.scm \
  modules/dom/element.scm \
  modules/dom/event.scm \
  modules/dom/image.scm \
  modules/dom/media.scm \
  modules/dom/window.scm \
  modules/math.scm \
  modules/math/rect.scm \
  modules/math/vector.scm \
  modules/game/input.scm \
  modules/game/physics.scm \
  modules/game/render.scm \
  modules/game/state.scm

game.wasm: game.scm $(modules)
	guild compile-wasm -L modules -o $@ $<

serve: game.wasm
	guile -c '((@ (hoot web-server) serve))'



bundle: game.wasm
	rm malaborio.zip || true
	zip malaborio.zip -r assets/ js-runtime/ game.js game.css game.wasm index.html

clean:
	rm -f game.wasm malaborio.zip
