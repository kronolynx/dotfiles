pragma Singleton

import Quickshell
// for the `color` property type
import QtQuick

// Palette and geometry ported 1:1 from the macOS sketchybar config
// (dots/Darwin/config/sketchybar/{colors.sh,sketchybarrc,items/*.sh}) so both
// machines look the same.
Singleton {
    id: root

    // ---- Palette: verbatim from sketchybar/colors.sh ----
    readonly property color black: "#151A22"
    readonly property color white: "#FAFAFA"
    readonly property color red: "#DF6E6F"
    readonly property color green: "#7EBDAC"
    readonly property color blue: "#5CA4C3"
    readonly property color yellow: "#FECC71"
    readonly property color orange: "#FEA361"
    readonly property color magenta: "#9B6BBF"
    readonly property color grey: "#8991A2"
    readonly property color dgrey: "#3F4965"
    readonly property color cyan: "#6ED7D6"

    // ---- Geometry: sketchybarrc bar{} + items' background.height ----
    readonly property int barHeight: 29      // bar height=29
    readonly property int barOffset: 4       // bar y_offset=4
    readonly property int barMargin: 5       // bar margin=5
    readonly property int cellHeight: 23     // items' background.height=23
    readonly property int radius: 3          // bar corner_radius=3
    readonly property int islandPad: 3       // bracket border_width=3
    readonly property int cellGap: 4         // items' padding_left/right=2, doubled
    readonly property int iconPad: 5         // icon.padding_left/right=5
    readonly property int labelPad: 5

    // Total space the bar takes from the top of the screen.
    readonly property int barSpace: barHeight + barOffset

    // ---- Fonts ----
    // sketchybar uses "SF Pro" (Mac only); Inter is the closest thing packaged
    // for Arch. Icons come from a Nerd Font instead of SF Symbols.
    //
    // Each is resolved against the fonts actually installed, so the bar renders
    // correctly before `inter-font` is installed and picks Inter up by itself
    // afterwards.
    readonly property string labelFont: root.firstInstalled(["Inter", "Noto Sans", "DejaVu Sans"])
    readonly property string iconFont: root.firstInstalled(["JetBrainsMono Nerd Font", "Symbols Nerd Font", "JetBrainsMono Nerd Font Mono"])

    function firstInstalled(candidates) {
        const installed = Qt.fontFamilies();
        for (const candidate of candidates)
            if (installed.includes(candidate))
                return candidate;
        return candidates[candidates.length - 1];
    }
    readonly property int labelSize: 13
    readonly property int iconSize: 14

    // ---- Behaviour ----
    readonly property string wallpaper: `${Quickshell.env("HOME")}/.wallpapers/amontTrees.jpg`
    readonly property string backlightDevice: "nvidia_wmi_ec_backlight"
    // Show only this monitor's workspaces on each bar (false shows all of them).
    readonly property bool workspacesPerMonitor: true
    readonly property int popupRadius: 6
    readonly property int popupPad: 10

    // ---- Popup / list colours ----
    readonly property color popupBg: "#1B2029"
    readonly property color rowHover: "#252C38"
}
