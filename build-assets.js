const http = require("http");
const fs = require("fs");
const Pageres = require('pageres');
const packageJson = require("./package.json");
const revision = require('child_process')
    .execSync('git rev-parse --short HEAD')
    .toString().trim()
const port = 3000;


const server = http.createServer((req, res) => {
    // `${process.env.GAME}_bundle.js`;
    if (req.url === "/") {
        req.url = `/index.html`;
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

server.listen(port, () => {
    console.log("server started");
    screenshot(() => server.close());
});


function screenshot(done) {
    const url = `http://localhost:${port}/`;
    const preview = new Pageres({
        filename: "preview",
        delay: 1,
        css: `body::after{content: "Version: ${packageJson.version} (${revision})"; position:absolute; bottom: 10px; left:10px}`
    })
        .src(url, ['1200x675'])
        .dest(`${__dirname}/gh-pages`)
        .run();
    preview.then(done);
}

