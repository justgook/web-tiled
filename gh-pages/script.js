function loadScript(src) {
    return new Promise(function (resolve, reject) {
        var s;
        s = document.createElement("script");
        s.src = src;
        s.onload = resolve;
        s.onerror = reject;
        document.head.appendChild(s);
    });
}

function initRemoteStorage() {
    return loadScript("https://unpkg.com/remotestoragejs@1.2.2/release/remotestorage.js")
        .then(() => {
            const dataPath = "web-tiled";
            const remoteStorage = new RemoteStorage({ modules: [WebTiled] });
            // remoteStorage.setApiKeys({  googledrive: "AIzaSyAiwd13yS5K1sD7c3iGOYCczMas3sK2yVE"  })
            remoteStorage.access.claim(dataPath, "rw");
            remoteStorage.caching.enable(`/${dataPath}/`);
            const client = remoteStorage.scope(`/${dataPath}/`);
            // remoteStorage["web-tiled"].listFiles()
            remoteStorage.api = remoteStorage["web-tiled"];
            return remoteStorage;
            // return remoteStorage["web-tiled"]
            //     .getConfig(defaultConfig)
            //     .then((config) => ({ client, remoteStorage, config }));

        });
}

function createWidget(remoteStorage) {
    loadScript("https://unpkg.com/remotestorage-widget@1.4.0/build/widget.js").then(() => {
        const widget = new Widget(remoteStorage);
        widget.attach();
        const widgetElement = document.getElementById("remotestorage-widget")
        widgetElement.style.bottom = 0;
        widgetElement.style.right = 0;
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

        function rpc(request, response) {
            request(({ method, params, id }) => {

                exports[method].apply(null, params)
                    .then((data) => {
                        // console.log(method, params, id, data);
                        response({ id, data })
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
