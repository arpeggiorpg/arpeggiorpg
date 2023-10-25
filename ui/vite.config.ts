import react from "@vitejs/plugin-react-swc";
import analyze from "rollup-plugin-analyzer";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  base: "/",
  plugins: [react(), analyze({ limit: 20 })],
});
