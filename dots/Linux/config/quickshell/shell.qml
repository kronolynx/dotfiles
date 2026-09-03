import Quickshell
import qs.bar
import qs.wallpaper

// A bar and a wallpaper per output.
ShellRoot {
    Variants {
        model: Quickshell.screens

        Scope {
            id: scope

            required property var modelData

            Wallpaper {
                modelData: scope.modelData
            }

            Bar {
                modelData: scope.modelData
            }
        }
    }
}
