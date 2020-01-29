import {getParameterByName, loadScript} from "./util.mjs";
import * as BuildWorker from "./build.mjs";
import {initRemoteStorage} from "./remotestorage.mjs";

document.addEventListener("dragover", function (event) {
    event.preventDefault();
}, false);
document.addEventListener("drop", function (event) {
    event.preventDefault();
}, false);

document.body.addEventListener("dragstart", event => {
    if (event.target && event.target.draggable) {
        event.dataTransfer.setData("text/html", "blank");
    }
});

document.body.addEventListener("dragover", event => {
    // this is needed in order to make dragging work
    return false;
});

//TODO add filtering for specific keys - like `⌘S` `⌘R`
// document.body.addEventListener("keydown", event => {
//     event.preventDefault();
//     // event.stopPropagation();
// }, false);

const flags = {
    version: "#VERSION#",
    levelUrl: getParameterByName("url") || "top-down-adventure/demo.json"
};


/// BUILD WORKER
const buildWorker = BuildWorker.init();

const defaultConfig = { file: "hello.json" };

export default async function (version) {
    await loadScript("bundle.js");
    ////----APPLICATION -----
    flags.version = version;
    const app = Object.values(Elm)[0].init({
        node: document.querySelector("main"),
        flags: flags
    });

    app.ports.build.subscribe(({ level, build, run }) => {
        console.time("buildWorker");
        buildWorker.postMessage({ build: build, run: run, level })
    });
    const remoteStorage = await initRemoteStorage();

    const { api } = remoteStorage;
    api.getConfig(defaultConfig).then(() => {
            console.log(app.ports);
            // createWidget(remoteStorage);
            // api.rpc(app.ports.toStore.subscribe, app.ports.fromStore.send);
        }
    );
}
