tailwind:
  npx tailwindcss --watch -i ./tailwind/input.css -o ./static/css/tw.css

servelive target="website":
  watchexec --exts hs,css,html,md -c -r cabal run {{target}}
