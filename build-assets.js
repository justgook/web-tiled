const http = require("http");
const fs = require("fs");
const Pageres = require('pageres');
const port = 3000;

const server = http.createServer((req, res) => {
    // `${process.env.GAME}_bundle.js`;
    if (req.url === "/") {
        req.url = `/index.html`;
    } else if (/bundle\.js$/.test(req.url)) {
        req.url = `/${process.env.GAME}_bundle.js`;
    } else if (req.url === `/${process.env.GAME}.age.bin` || req.url === "/default.age.bin") {
        req.url = `/${process.env.GAME}.age.bin`;
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


// function screenshot(done) {
//     const url = `http://localhost:${port}/`;
//     const savePreview = `gh-pages/${process.env.GAME}.png`;
//     const preview = new Promise((resolve, reject) =>
//         takeScreenShot.fromURL(url, savePreview,
//             {
//                 show: true,
//                 width: 1200,
//                 height: 675,
//                 waitAfterSelector: "body > *",
//                 waitMilliseconds: 100,
//             },
//             () => {
//                 console.log(`Screenshot: ${savePreview}`);
//                 resolve()
//             }
//         ));
//     preview.then(done);
// }

function screenshot(done) {
    const url = `http://localhost:${port}/`;
    const preview = new Pageres({
        filename: process.env.GAME,
        delay: 0
    })
        .src(url, ['1200x675'])
        .dest(`${__dirname}/gh-pages`)
        .run();
    preview.then(done);
}

