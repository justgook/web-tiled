export const run = function (obj) {
    return run_(obj, 0)
};

export function getParameterByName(name) {
    var match = RegExp("[?&]" + name + "=([^&]*)").exec(window.location.search);
    return match && decodeURIComponent(match[1].replace(/\+/g, " "));
}


export function loadScript(src) {
    return new Promise(function (resolve, reject) {
        const s = document.createElement("script");
        s.src = src;
        s.onload = resolve;
        s.onerror = reject;
        document.head.appendChild(s);
    });
}

function run_(obj, i) {
    if (i > 5) {
        return;
    }
    const ref = obj[Object.keys(obj)[0]];
    return ref.init
        ? ref.init
        : run_(ref, ++i);
}
