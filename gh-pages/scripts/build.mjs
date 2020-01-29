

function openRunWindow(w, h, flags, file) {
    const left = (screen.availLeft || 0) + (screen.width / 2) - (w / 2);
    const top = (screen.availTop || 0) + (screen.height / 2) - (h / 2);
    window.getFlags = function () {
        return {flags, file};
    };
    const win = window.open("game.html", "run-window", `directories=no,titlebar=no,toolbar=no,location=no,status=no,menubar=no,scrollbars=no,resizable=no,width=${w},height=${h},top=${top},left=${left}`, false);
    window.onbeforeunload = () => win.close();
}

export function init () {
    const buildWorker = new Worker("scripts/worker.js");

    buildWorker.onmessage = function (e) {
        console.timeEnd("buildWorker");
        openRunWindow(800, 600, e.data.data, e.data.run);
    };

    return buildWorker;
}
