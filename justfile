tailwind:
  npx tailwindcss --watch -i ./tailwind/input.css -o ./static/css/tw.css

servelive target="website":
  watchexec --exts hs,cabal,css,html,md -c -r cabal run {{target}}

htmx:
  npm ci
  cp node_modules/htmx.org/dist/htmx.js static # todo: minified
