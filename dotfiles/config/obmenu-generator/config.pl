#!/usr/bin/perl

# obmenu-generator - configuration file
# This file will be updated automatically.
# Any additional comment and/or indentation will be lost.

=for comment

|| FILTERING
    | skip_filename_re    : Skip a .desktop file if its name matches the regex.
                            Name is from the last slash to the end. (e.g.: name.desktop)
                            Example: qr/^(?:gimp|xterm)\b/,    # skips 'gimp' and 'xterm'

    | skip_entry          : Skip a desktop file if the value from a given key matches the regex.
                            Example: [
                                {key => 'Name',       re => qr/(?:about|terminal)/i},
                                {key => 'Exec',       re => qr/^xterm/},
                                {key => 'OnlyShowIn', re => qr/XFCE/},
                            ],

    | substitutions       : Substitute, by using a regex, in the values from the desktop files.
                            Example: [
                                {key => 'Exec', re => qr/xterm/, value => 'tilix', global => 1},
                            ],

|| ICON SETTINGS
    | use_gtk3            : Use the Gtk3 library for resolving the icon paths. (default: 0)
    | gtk_rc_filename     : Absolute path to the GTK configuration file.
    | missing_icon        : Use this icon for missing icons (default: gtk-missing-image)
    | icon_size           : Preferred size for icons. (default: 48)
    | generic_fallback    : Try to shorten icon name at '-' characters before looking at inherited themes. (default: 0)
    | force_icon_size     : Always get the icon scaled to the requested size. (default: 0)

|| PATHS
    | desktop_files_paths   : Absolute paths which contain .desktop files.
                              Example: [
                                '/usr/share/applications',
                                "$ENV{HOME}/.local/share/applications",
                                glob("$ENV{HOME}/.local/share/applications/wine/Programs/*"),
                              ],

|| NOTES
    | Regular expressions:
        * use qr/.../ instead of '...'
        * use qr/.../i for case insensitive mode

=cut

our $CONFIG = {
  "editor"              => "geany",
  "force_icon_size"     => 0,
  "generic_fallback"    => 0,
  "gtk_rc_filename"     => "$ENV{HOME}/.gtkrc-2.0",
  "icon_size"           => 32,
  "Linux::DesktopFiles" => {
                             desktop_files_paths     => [
                                                          "/usr/share/applications",
                                                          "/usr/local/share/applications",
                                                          "/usr/share/applications/kde4",
                                                          "$ENV{HOME}/.local/share/applications",
                                                        ],
                             keep_unknown_categories => 1,
                             skip_entry              => [
                                                          { key => "Name", re => qr/About Xfce/ },
                                                          { key => "Name", re => qr/LightDM*/ },
                                                          { key => "Name", re => qr/compton*/ },
                                                          { key => "Name", re => qr/Avahi Zeroconf*/ },
                                                          { key => "Name", re => qr/GParted/ },
                                                          { key => "Comment[fr]", re => "Panel l\xC3\xA9ger" },
                                                        ],
                             skip_filename_re        => undef,
                             substitutions           => [
                                                          {
                                                            key => "Name",
                                                            re => qr/GNU Image Manipulation Program/i,
                                                            value => "GIMP",
                                                          },
                                                          {
                                                            key => "Name",
                                                            re => qr/PulseAudio Volume Control/i,
                                                            value => "Pulse Audio",
                                                          },
                                                          {
                                                            key => "Name",
                                                            re => qr/Avahi SSH Server Browser/i,
                                                            value => "Avahi SSH",
                                                          },
                                                          {
                                                            key => "Name",
                                                            re => qr/Avahi VNC Server Browser/i,
                                                            value => "Avahi VNC",
                                                          },
                                                          {
                                                            key => "Name",
                                                            re => qr/Openbox Configuration Manager/i,
                                                            value => "Openbox Configs",
                                                          },
                                                          {
                                                            key => "Name",
                                                            re => qr/Customize Look and Feel/i,
                                                            value => "LX Appearance",
                                                          },
                                                          { key => "Name", re => qr/Monitor Settings/i, value => "LX RandR" },
                                                          {
                                                            key => "Name",
                                                            re => qr/Oomox: customize icons and GTK themes/i,
                                                            value => "Oomox",
                                                          },
                                                        ],
                             terminalization_format  => "%s -e '%s'",
                             terminalize             => 1,
                             unknown_category_key    => "other",
                           },
  "locale_support"      => 1,
  "missing_icon"        => "gtk-missing-image",
  "terminal"            => "termite",
  "use_gtk3"            => 0,
  "VERSION"             => 0.88,
}
