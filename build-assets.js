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
    screenshot(() => server.close());
});


function screenshot(done) {
    stepScreenshot()
        .catch(() => process.exit(-1))
        .then(done);
}

async function stepScreenshot() {
    const puppeteer = require('puppeteer');

    // 1. Launch the browser
    const browser = await puppeteer.launch();
    // 2. Open a new page
    const page = await browser.newPage();
    await stepStepSTEP(page);
    await browser.close();

}

async function stepStepSTEP(page) {
    const url = `http://localhost:${port}/`;
    console.log(`Screenshot capture`);
    // 3. Navigate to URL
    await page.goto(`${url}index.html`);
    await page.addStyleTag({content: `body::after{content: "Version: ${packageJson.version} (${revision})"; position:absolute; bottom: 5px; left:10px}`})


    await page.waitFor(4000);

    // 4. Take screenshot
    await page.screenshot({ omitBackground: true, path: `${__dirname}/gh-pages/preview.png` });

    if (input.length) {
        await stepStepSTEP(page, input);
    }

}
