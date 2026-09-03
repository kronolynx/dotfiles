import Quickshell
import Quickshell.Wayland
import QtQuick
import qs.theme

// Replaces swaybg. The path is declared statically in Config.qml, which is
// where the wallpaper choice belongs.
PanelWindow {
    id: root

    required property var modelData

    screen: modelData
    color: Config.black
    // Reserve nothing, and ignore what everything else reserves: without
    // Ignore the bar's own exclusive zone shrinks this surface to 0,33 and the
    // strip behind the bar stays empty.
    exclusionMode: ExclusionMode.Ignore

    WlrLayershell.layer: WlrLayer.Background
    WlrLayershell.namespace: "quickshell:wallpaper"

    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }

    Image {
        anchors.fill: parent
        source: `file://${Config.wallpaper}`
        fillMode: Image.PreserveAspectCrop
        asynchronous: true
        cache: false
        // Decode at the output's real pixel size rather than the logical one.
        sourceSize.width: root.screen ? root.screen.width * root.screen.devicePixelRatio : 0
        sourceSize.height: root.screen ? root.screen.height * root.screen.devicePixelRatio : 0

        onStatusChanged: {
            if (status === Image.Error)
                console.warn(`wallpaper: could not load ${Config.wallpaper}`);
        }
    }
}
