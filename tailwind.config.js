/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.hs"],
  theme: {
    colors: {
      transparent: "transparent",
      current: "currentColor",
      "white": '#ffffff',
      "brass": "#b5a642",
      "soft-blue": "#5264DF",
    },
    extend: {},
  },
  plugins: [
    require('@tailwindcss/typography'),
  ],
}
