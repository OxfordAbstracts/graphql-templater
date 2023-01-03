const { readFileSync } = require("fs");

let url = "";
let headers = "";
let query = "";

try {
  url = readFileSync("./dev_config/url.txt").toString();
} catch (e) {}
try {
  headers = readFileSync("./dev_config/headers.txt").toString();
} catch (e) {}
try {
  query = readFileSync("./dev_config/query.txt").toString();
} catch (e) {}

require("esbuild")
  .serve(
    {
      servedir: "dist",
    },
    {
      entryPoints: ["index.js"],
      bundle: true,
      define: {
        "process.env.url": JSON.stringify(url),
        "process.env.headers": JSON.stringify(headers),
        "process.env.query": JSON.stringify(query),
      },
    }
  )
  .then((server) => {
    console.log(`server running at http://localhost:${server.port}`);
  });
