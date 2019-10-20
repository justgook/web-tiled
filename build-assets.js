const http = require("http");
const fs = require("fs");
const takeScreenShot = require("node-server-screenshot");
const port = 3000;

const server = http.createServer((req, res) => {
    // `${process.env.GAME}_bundle.js`;
    if (req.url === "/") {
        req.url = `/index.html`;
    } else if (/bundle\.js$/.test(req.url)) {
        req.url = `/dist/${process.env.GAME}_bundle.js`;
    } else if (req.url === `/${process.env.GAME}.age.bin` || req.url === "/default.age.bin") {
        req.url = `/dist/${process.env.GAME}.age.bin`;
    } else if (fs.existsSync(__dirname + "/gh-pages/dist" + req.url)) {
        req.url = "/dist" + req.url;
    }

    const path = __dirname + "/gh-pages" + req.url;
    fs.access(path, fs.F_OK, (err) => {
        if (err) {
            console.error(err);
            res.writeHead(404);
            res.end();
            process.exit(-1);
            return;
        }
        res.writeHead(200);
        res.end(fs.readFileSync(path));
    })

});

server.listen(port, ()=> {
    console.log("server started")
    screenshot(() => server.close());
});



function screenshot(done) {
    const url = `http://localhost:${port}/`;
    const savePreview = `gh-pages/dist/${process.env.GAME}.png`;
    console.log("aa");
    const preview = new Promise((resolve, reject) =>

        takeScreenShot.fromURL(url, savePreview,
            {
                show: true,
                width: 1200,
                height: 675,
                waitAfterSelector: "body > *",
                waitMilliseconds: 100,
            },
            () => {
                console.log(`Screenshot: ${savePreview}`);
                resolve()
            }
        ));
     preview.then(done);
}
