const cache = new Map();
onmessage = function (e) {
    let fn = cache.get(e.data.build);
    if (!fn) {
        importScripts(e.data.build);
        fn = run(Elm);
        cache.set(e.data.build, fn);
    }
    const app = fn({ flags: { level: e.data.level } });

    app.ports.done.subscribe((a) => postMessage({ run: e.data.run, data: a }));
};


const run = function (obj) {
    return run_(obj, 0)
};

function run_(obj, i) {
    if (i > 5) {
        return;
    }
    const ref = obj[Object.keys(obj)[0]];
    return ref.init
        ? ref.init
        : run_(ref, ++i);
}
