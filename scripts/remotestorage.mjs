import {loadScript} from "./util.mjs";


export function initRemoteStorage() {
    return loadScript("https://unpkg.com/remotestoragejs@1.2.3/release/remotestorage.js")
        .then(() => {
            const dataPath = "web-tiled";
            const remoteStorage = new RemoteStorage({ modules: [WebTiled] });
            remoteStorage.setSyncInterval(60000);
            // remoteStorage.enableLog();
            remoteStorage.access.claim(dataPath, "rw");
            remoteStorage.caching.enable(`/${dataPath}/`);
            // https://remotestoragejs.readthedocs.io/en/latest/js-api/remotestorage.html#constructor
            remoteStorage.api = remoteStorage["web-tiled"];
            remoteStorage.api.rpc = remoteStorage.api.rpc.bind(remoteStorage);


            return remoteStorage;
        });
}

const WebTiled = {
    name: "web-tiled",
    builder: function (privateClient, publicClient) {
        const name = "web-tiled";
        const files = "files";
        privateClient.declareType("web-tiled-config", {
            "$schema": "http://json-schema.org/draft-04/schema#",
            "type": "object",
            "properties": {
                "file": {
                    "type": "string"
                }
            },
            "required": [
                "file"
            ]
        });

        function getConfig(defaultConfig) {
            return privateClient.getObject(".config")
                .then((gotConfig) => gotConfig ? gotConfig : setConfig(defaultConfig));
        }

        function setConfig(config) {
            return privateClient.storeObject("web-tiled-config", ".config", config)
                .then(function () {
                    return config;
                })
        }

        function getFile(path, maxAge) {
            return privateClient.getFile(`${files}/${path}`, maxAge);
        }

        function storeFile(mimeType, path, body) {
            return privateClient.storeFile(mimeType, `${files}/${path}`, body)
        }

        function getFiles(maxAge) {
            return Promise.all([
                privateClient.getListing(`${files}/`, maxAge).catch(() => ({})),
                publicClient.getListing(`${files}/`, maxAge).catch(() => ({}))
            ])
                .then(([inPrivate, inPublic]) => ({ private: inPrivate, public: inPublic }))

        }

        // privateClient.on('change', function (evt) {
        //     console.log('privateClient::change', evt);
        // });
        //
        // publicClient.on('change', function (evt) {
        //     console.log('publicClient::change', evt);
        // });

        function rpc(subscribe, send) {
            this.on("ready", (data) => send({ id: "ready", data }));
            this.on("not-connected", (data) => send({ id: "not-connected", data }));
            this.on("connected", () => send({ id: "connected", data: this.remote }));
            this.on("disconnected", (data) => send({ id: "disconnected", data }));
            this.on("error", (data) => send({ id: "error", data }));
            this.on("features-loaded", (data) => send({ id: "features-loaded", data }));
            this.on("connecting", (data) => send({ id: "connecting", data }));
            this.on("authing", (data) => send({ id: "authing", data }));
            this.on("wire-busy", (data) => send({ id: "wire-busy", data }));
            this.on("wire-done", (data) => send({ id: "wire-done", data }));
            this.on("sync-req-done", (data) => send({ id: "sync-req-done", data }));
            this.on("sync-done", (data) => send({ id: "sync-done", data }));
            this.on("network-offline", (data) => send({ id: "network-offline", data }));
            this.on("network-online", (data) => send({ id: "network-online", data }));
            this.on("sync-interval-change", (data) => send({ id: "sync-interval-change", data }));
            exports.connect = (data) => Promise.resolve(this.connect(data));
            exports.disconnect = (data) => Promise.resolve(this.disconnect(data));
            subscribe(({ method, params, id }) => {
                exports[method].apply(this, params)
                    .then((data) => {
                        send({ id, data })
                    })
            });
        }

        const exports = {
            rpc: rpc,
            getConfig: getConfig,
            setConfig: setConfig,
            getFiles: getFiles,
            getFile: getFile,
            storeFile: storeFile,
        };
        return { exports }
    }
};
