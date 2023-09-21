type ElmPagesInit = {
  load: (elmLoaded: Promise<unknown>) => Promise<void>;
  flags: { width: number, colorScheme: "Dark" | "Light" };
};

const config: ElmPagesInit = {
  load: async function (elmLoaded) {
    await elmLoaded;
  },
  flags: function () {
    const useDarkTheme = window.matchMedia("(prefers-color-scheme: dark)").matches;
    const colorScheme = useDarkTheme ? "Dark" : "Light";
    return { width: window.innerWidth, colorScheme };
  },
};

export default config;
