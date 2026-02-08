import { Suspense } from "react";
import App from "./App";

export default function Home() {
  return (
    <Suspense fallback={<div className="flex min-h-screen items-center justify-center bg-slate-100 dark:bg-slate-950">Loading...</div>}>
      <App />
    </Suspense>
  );
}
