module.exports = {
  content: ["./src/**/*.{html,js,jsx,ts,tsx,elm}"],
  theme: {
    extend: {
      transitionProperty: {
        'height': 'height',
      }
    },
  },
  plugins: [require("@tailwindcss/typography")],
}