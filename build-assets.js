const http = require("http");
const fs = require("fs");
const packageJson = require("./package.json");
const revision = require('child_process')
    .execSync('git rev-parse --short HEAD')
    .toString().trim();
const port = 3000;

const root = "gh-pages";

const server = http.createServer((req, res) => {
    // `${process.env.GAME}_bundle.js`;
    if (req.url === "/") {
        req.url = `/index.html`;
    }

    const path = __dirname + `/${root}` + req.url;
    fs.access(path, fs.F_OK, (err) => {
        if (err) {
            console.error(err);
            res.writeHead(404);
            res.end();
            process.exit(-1);
            return;
        }
        res.writeHead(200,
            path.endsWith(".mjs")
                ? { "Content-Type": "application/javascript; charset=utf-8" }
                : {}
        );
        res.end(fs.readFileSync(path));
    })

});

server.listen(port, () => {
    console.log("server started");
    screenshot()
        .catch(() => process.exit(-1))
        .then(() => server.close());
});



async function screenshot() {
    console.log("init puppeteer");
    const puppeteer = require('puppeteer');
    console.log("1. Launch the browser");
    let browser = null;
    try {
        browser = await puppeteer.launch();
    } catch (e) {
        console.log(`Can not launch puppeteer: ${e}`);
        process.exit(-1);
    }
    console.log("2. Open a new page");
    const page = await browser.newPage();
    await stepStepSTEP(page);
    await browser.close();

}

async function stepStepSTEP(page) {
    const url = `http://localhost:${port}/`;
    console.log(`3. Navigate to URL`);
    await page.goto(`${url}index.html`);
    console.log(`3.1 Add style`);
    await page.addStyleTag({content: `body::after{content: "Version: ${packageJson.version} (${revision})"; position:absolute; bottom: 5px; left:10px}`});
    console.log(`3.2 Wait for page renders (4s)`);
    await page.waitFor(4000);
    console.log("4. Take screenshot");
    return await page.screenshot({ omitBackground: true, path: `${__dirname}/gh-pages/preview.png` });

}
