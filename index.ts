type ElmPagesInit = {
  load: (elmLoaded: Promise<unknown>) => Promise<void>;
  flags: { colorScheme: "Dark" | "Light" };
};

const config: ElmPagesInit = {
  load: async function (elmLoaded) {
    const app = await elmLoaded;
    app.ports.newTab.subscribe(url => window.open(url, "_blank"));
  },
  flags: function () {
    const useDarkTheme = window.matchMedia("(prefers-color-scheme: dark)").matches;
    const colorScheme = useDarkTheme ? "Dark" : "Light";
    return { colorScheme };
  },
};

export default config;
