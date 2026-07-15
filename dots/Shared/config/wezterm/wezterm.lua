-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()
-- config.set_environment_variables = {
--     XDG_CONFIG_HOME = os.getenv("HOME") .. "/.config",
--   }

-- config.default_prog = { "/opt/homebrew/bin/nu", "-l" }
-- config.default_prog = { '/opt/homebrew/bin/tmux' }

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'Catppuccin Frappe'
-- wezterm ls-fonts --list-system
-- config.font = wezterm.font 'Fantasque Sans Mono'
-- config.font = wezterm.font 'Cascadia Code NF'
-- config.font = wezterm.font 'Rec Mono Duotone'
-- config.font = wezterm.font("Iosevka", { weight = "Medium" })
-- config.font = wezterm.font("FiraCodeGG Nerd Font", { weight = "Medium" })
-- config.font = wezterm.font("Iosevka GG", { stretch = "Expanded", weight = "Medium" })
-- config.font = wezterm.font({ family = "VictorMono Nerd Font", weight = 500, harfbuzz_features = { "ss01=off" } })
config.font = wezterm.font({ family = "Victor Mono", weight = 600, harfbuzz_features = {} })
-- config.font = wezterm.font("Maple Mono", { weight = "Medium" })
-- config.font = wezterm.font({ family = "Rec Mono Duotone", weight = "Medium" })
-- config.font = wezterm.font({ family = "CaskaydiaCove Nerd Font", weight = "Medium" })
-- config.font = wezterm.font({ family = "Dank Mono" })
-- config.font = wezterm.font({ family = "Fantasque Sans Mono" })
-- config.font = wezterm.font({ family = "CommitMono-GG" })
-- config.font = wezterm.font({ family = "Mononoki" })
config.font_size = 13
-- config.font = wezterm.font('Victor Mono', { weight = 'SemiBold' })
config.use_fancy_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.window_decorations = "RESIZE | MACOS_FORCE_DISABLE_SHADOW"
config.window_background_opacity = 0.91
config.macos_window_background_blur = 10
config.scrollback_lines = 50000
config.adjust_window_size_when_changing_font_size = false
config.use_resize_increments = false
config.allow_square_glyphs_to_overflow_width = 'Always'
config.front_end = 'WebGpu'
config.webgpu_power_preference = 'HighPerformance'
config.window_close_confirmation = 'NeverPrompt'

-- config.send_composed_key_when_left_alt_is_pressed = true
config.send_composed_key_when_left_alt_is_pressed = false
config.send_composed_key_when_right_alt_is_pressed = false

config.window_padding = {
  left = 5,
  right = 5,
  top = 5,
  bottom = 0,
}

local act = wezterm.action
local mods = 'SHIFT|CTRL'
-- local leader = 'LEADER'

-- timeout_milliseconds defaults to 1000 and can be omitted
config.leader = { key = 'y', mods = 'CTRL', timeout_milliseconds = 1000 }
-- get current keyz `wezterm show-keys --lua`
config.disable_default_key_bindings = true
config.keys = {
  { mods = mods, key = 'x',           action = act.ActivateCopyMode },
  { key = 'c', mods = 'CMD',          action = act.CopyTo 'Clipboard' },
  { key = 'v', mods = 'CMD',          action = act.PasteFrom 'Clipboard' },
  { key = 'c', mods = 'SHIFT|CTRL',   action = act.CopyTo 'Clipboard' },
  { key = 'v', mods = 'SHIFT|CTRL',   action = act.PasteFrom 'Clipboard' },
  { key = '+',   mods = 'SHIFT|CTRL', action = act.IncreaseFontSize },
  { key = '-',   mods = 'SHIFT|CTRL', action = act.DecreaseFontSize },
  { key = '0',   mods = 'SHIFT|CTRL', action = act.ResetFontSize },
  {
    key = 'K',
    mods = 'CTRL|SHIFT',
    action = act.ClearScrollback 'ScrollbackOnly',
  },
  -- {
  --   key = 'K',
  --   mods = 'CTRL|SHIFT',
  --   action = act.Multiple {
  --     act.ClearScrollback 'ScrollbackAndViewport',
  --     act.SendKey { key = 'L', mods = 'CTRL' },
  --   },
  -- },
  -- { mods = leader, key = 'd',          action = act.ShowDebugOverlay },
  -- { mods = leader, key = 'v',          action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
  -- { mods = leader, key = 's',          action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
  -- { mods = leader, key = 'h',          action = act.ActivatePaneDirection 'Left' },
  -- { mods = leader, key = 'l',          action = act.ActivatePaneDirection 'Right' },
  -- { mods = leader, key = 'k',          action = act.ActivatePaneDirection 'Up' },
  -- { mods = leader, key = 'j',          action = act.ActivatePaneDirection 'Down' },
  -- { mods = leader, key = 't',          action = act.SpawnTab 'CurrentPaneDomain' },
  -- { mods = leader, key = 'n',          action = act.SpawnWindow },
  -- { mods = leader, key = 'q',          action = act.CloseCurrentPane { confirm = true } },
  -- { mods = leader, key = '1',          action = act.ActivateTab(0) },
  -- { mods = leader, key = '2',          action = act.ActivateTab(1) },
  -- { mods = leader, key = '3',          action = act.ActivateTab(2) },
  -- { mods = leader, key = '4',          action = act.ActivateTab(3) },
  -- { mods = leader, key = '5',          action = act.ActivateTab(4) },
  -- { mods = leader, key = '6',          action = act.ActivateTab(5) },
  -- { mods = leader, key = '7',          action = act.ActivateTab(6) },
  -- { mods = leader, key = '8',          action = act.ActivateTab(7) },
  -- { mods = leader, key = '9',          action = act.ActivateTab(8) },
  -- { mods = mods,   key = 'RightArrow', action = act.ActivateTabRelative(1) },
  -- { mods = mods,   key = 'LeftArrow',  action = act.ActivateTabRelative(-1) },

  -- { key = 'Tab', mods = 'CTRL', action = act.ActivateTabRelative(1) },
  -- { key = 'Tab', mods = 'SHIFT|CTRL', action = act.ActivateTabRelative(-1) },
  -- { key = 'Tab', mods = 'CTRL', action = act.ActivateTabRelative(1) },
  -- { key = 'Tab', mods = 'SHIFT|CTRL', action = act.ActivateTabRelative(-1) },
  -- { key = 'Enter', mods = 'ALT', action = act.DisableDefaultAssignment }, --, action = act.ToggleFullScreen },
  -- { key = '!', mods = 'CTRL', action = act.ActivateTab(0) },
  -- { key = '!', mods = 'SHIFT|CTRL', action = act.ActivateTab(0) },
  -- { key = '\"', mods = 'ALT|CTRL', action = act.SplitVertical{ domain =  'CurrentPaneDomain' } },
  -- { key = '\"', mods = 'SHIFT|ALT|CTRL', action = act.SplitVertical{ domain =  'CurrentPaneDomain' } },
  -- { key = '#', mods = 'CTRL', action = act.ActivateTab(2) },
  -- { key = '#', mods = 'SHIFT|CTRL', action = act.ActivateTab(2) },
  -- { key = '$', mods = 'CTRL', action = act.ActivateTab(3) },
  -- { key = '$', mods = 'SHIFT|CTRL', action = act.ActivateTab(3) },
  -- { key = '%', mods = 'CTRL', action = act.ActivateTab(4) },
  -- { key = '%', mods = 'SHIFT|CTRL', action = act.ActivateTab(4) },
  -- { key = '%', mods = 'ALT|CTRL', action = act.SplitHorizontal{ domain =  'CurrentPaneDomain' }                      },
  -- { key = '%', mods = 'SHIFT|ALT|CTRL', action = act.SplitHorizontal{ domain =  'CurrentPaneDomain' } },
  -- { key = '&', mods = 'CTRL', action = act.ActivateTab(6) },
  -- { key = '&', mods = 'SHIFT|CTRL', action = act.ActivateTab(6) },
  -- { key = '\'', mods = 'SHIFT|ALT|CTRL', action = act.SplitVertical{ domain =  'CurrentPaneDomain' } },
  -- { key = '(', mods = 'CTRL', action = act.ActivateTab(-1) },
  -- { key = '(', mods = 'SHIFT|CTRL', action = act.ActivateTab(-1) },
  -- { key = ')', mods = 'CTRL', action = act.ResetFontSize },
  -- { key = ')', mods = 'SHIFT|CTRL', action = act.ResetFontSize },
  -- { key = '*', mods = 'CTRL', action = act.ActivateTab(7) },
  -- { key = '*', mods = 'SHIFT|CTRL', action = act.ActivateTab(7) },
  -- { key = '+', mods = 'CTRL', action = act.IncreaseFontSize },
  -- { key = '-', mods = 'CTRL', action = act.DecreaseFontSize },
  -- { key = '-', mods = 'SUPER', action = act.DecreaseFontSize },
  -- { key = '0', mods = 'CTRL', action = act.ResetFontSize },
  -- { key = '0', mods = 'SUPER', action = act.ResetFontSize },
  -- { key = '1', mods = 'SHIFT|CTRL', action = act.ActivateTab(0) },
  -- { key = '1', mods = 'SUPER', action = act.ActivateTab(0) },
  -- { key = '2', mods = 'SHIFT|CTRL', action = act.ActivateTab(1) },
  -- { key = '2', mods = 'SUPER', action = act.ActivateTab(1) },
  -- { key = '3', mods = 'SHIFT|CTRL', action = act.ActivateTab(2) },
  -- { key = '3', mods = 'SUPER', action = act.ActivateTab(2) },
  -- { key = '4', mods = 'SHIFT|CTRL', action = act.ActivateTab(3) },
  -- { key = '4', mods = 'SUPER', action = act.ActivateTab(3) },
  -- { key = '4', mods = 'SHIFT|ALT|CTRL', action = act.SplitVertical{ domain =  'CurrentPaneDomain' } },
  -- { key = '5', mods = 'SHIFT|CTRL', action = act.ActivateTab(4) },
  -- { key = '5', mods = 'SHIFT|ALT|CTRL', action = act.SplitHorizontal{ domain =  'CurrentPaneDomain' } },
  -- { key = '5', mods = 'SUPER', action = act.ActivateTab(4) },
  -- { key = '6', mods = 'SHIFT|CTRL', action = act.ActivateTab(5) },
  -- { key = '6', mods = 'SUPER', action = act.ActivateTab(5) },
  -- { key = '7', mods = 'SHIFT|CTRL', action = act.ActivateTab(6) },
  -- { key = '7', mods = 'SUPER', action = act.ActivateTab(6) },
  -- { key = '8', mods = 'SHIFT|CTRL', action = act.ActivateTab(7) },
  -- { key = '8', mods = 'SUPER', action = act.ActivateTab(7) },
  -- { key = '9', mods = 'SHIFT|CTRL', action = act.ActivateTab(-1) },
  -- { key = '9', mods = 'SUPER', action = act.ActivateTab(-1) },
  -- { key = '=', mods = 'CTRL', action = act.IncreaseFontSize },
  -- { key = '=', mods = 'SHIFT|CTRL', action = act.IncreaseFontSize },
  -- { key = '=', mods = 'SUPER', action = act.IncreaseFontSize },
  -- { key = '@', mods = 'CTRL', action = act.ActivateTab(1) },
  -- { key = '@', mods = 'SHIFT|CTRL', action = act.ActivateTab(1) },
  -- { key = 'C', mods = 'CTRL', action = act.CopyTo 'Clipboard' },
  -- { key = 'C', mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },
  -- { key = 'F', mods = 'CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },
  -- { key = 'F', mods = 'SHIFT|CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },
  -- { key = 'H', mods = 'CTRL', action = act.DisableDefaultAssignment },
  -- { key = 'H', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
  -- { key = 'K', mods = 'CTRL', action = act.ClearScrollback 'ScrollbackOnly' },
  -- { key = 'K', mods = 'SHIFT|CTRL', action = act.ClearScrollback 'ScrollbackOnly' },
  -- { key = 'L', mods = 'CTRL', action = act.ShowDebugOverlay },
  -- { key = 'L', mods = 'SHIFT|CTRL', action = act.ShowDebugOverlay },
  -- { key = 'M', mods = 'CTRL', action = act.Hide },
  -- { key = 'M', mods = 'SHIFT|CTRL', action = act.Hide },
  -- { key = 'N', mods = 'CTRL', action = act.SpawnWindow },
  -- { key = 'N', mods = 'SHIFT|CTRL', action = act.SpawnWindow },
  -- { key = 'P', mods = 'CTRL', action = act.ActivateCommandPalette },
  -- { key = 'P', mods = 'SHIFT|CTRL', action = act.ActivateCommandPalette },
  -- { key = 'Q', mods = 'CTRL', action = act.QuitApplication },
  -- { key = 'Q', mods = 'SHIFT|CTRL', action = act.QuitApplication },
  -- { key = 'R', mods = 'CTRL', action = act.ReloadConfiguration },
  -- { key = 'R', mods = 'SHIFT|CTRL', action = act.ReloadConfiguration },
  -- { key = 'T', mods = 'CTRL', action = act.SpawnTab 'CurrentPaneDomain' },
  -- { key = 'T', mods = 'SHIFT|CTRL', action = act.SpawnTab 'CurrentPaneDomain' },
  -- { key = 'U', mods = 'CTRL', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
  -- { key = 'U', mods = 'SHIFT|CTRL', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
  -- { key = 'V', mods = 'CTRL', action = act.PasteFrom 'Clipboard' },
  -- { key = 'V', mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
  -- { key = 'W', mods = 'CTRL', action = act.CloseCurrentTab{ confirm = true } },
  -- { key = 'W', mods = 'SHIFT|CTRL', action = act.CloseCurrentTab{ confirm = true } },
  -- { key = 'X', mods = 'CTRL', action = act.ActivateCopyMode },
  -- { key = 'X', mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },
  -- { key = 'Z', mods = 'CTRL', action = act.TogglePaneZoomState },
  -- { key = 'Z', mods = 'SHIFT|CTRL', action = act.TogglePaneZoomState },
  -- { key = '[', mods = 'SHIFT|SUPER', action = act.ActivateTabRelative(-1) },
  -- { key = ']', mods = 'SHIFT|SUPER', action = act.ActivateTabRelative(1) },
  -- { key = '^', mods = 'CTRL', action = act.ActivateTab(5) },
  -- { key = '^', mods = 'SHIFT|CTRL', action = act.ActivateTab(5) },
  -- { key = '_', mods = 'CTRL', action = act.DecreaseFontSize },
  -- { key = '_', mods = 'SHIFT|CTRL', action = act.DecreaseFontSize },
  -- { key = 'a', mods = 'CTRL|LEADER', action = act.SendKey{ key =  'a', mods =  'CTRL' } },
  -- { key = 'c', mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },
  -- { key = 'c', mods = 'SUPER', action = act.CopyTo 'Clipboard' },
  -- { key = 'f', mods = 'SHIFT|CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },
  -- { key = 'f', mods = 'SUPER', action = act.Search 'CurrentSelectionOrEmptyString' },
  -- -- { key = 'h', mods = 'SHIFT|CTRL', action = act.HideApplication },
  -- { key = 'h', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
  -- { key = 'h', mods = 'SUPER', action = act.DisableDefaultAssignment },
  -- -- { key = 'k', mods = 'SHIFT|CTRL', action = act.ClearScrollback 'ScrollbackOnly' },
  -- { key = 'k', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
  -- { key = 'k', mods = 'SUPER', action = act.ClearScrollback 'ScrollbackOnly' },
  -- -- { key = 'l', mods = 'SHIFT|CTRL', action = act.ShowDebugOverlay },
  -- { key = 'l', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
  -- { key = 'm', mods = 'SHIFT|CTRL', action = act.Hide },
  -- { key = 'm', mods = 'SUPER', action = act.Hide },
  -- { key = 'n', mods = 'SHIFT|CTRL', action = act.SpawnWindow },
  -- { key = 'n', mods = 'SUPER', action = act.SpawnWindow },
  -- { key = 'p', mods = 'SHIFT|CTRL', action = act.ActivateCommandPalette },
  -- { key = 'q', mods = 'SHIFT|CTRL', action = act.QuitApplication },
  -- { key = 'q', mods = 'SUPER', action = act.QuitApplication },
  -- { key = 'r', mods = 'SHIFT|CTRL', action = act.ReloadConfiguration },
  -- { key = 'r', mods = 'SUPER', action = act.ReloadConfiguration },
  -- { key = 't', mods = 'SHIFT|CTRL', action = act.SpawnTab 'CurrentPaneDomain' },
  -- { key = 't', mods = 'SUPER', action = act.SpawnTab 'CurrentPaneDomain' },
  -- { key = 'u', mods = 'SHIFT|CTRL', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
  -- { key = 'v', mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
  -- { key = 'v', mods = 'SUPER', action = act.PasteFrom 'Clipboard' },
  -- { key = 'w', mods = 'SHIFT|CTRL', action = act.CloseCurrentTab{ confirm = true } },
  -- { key = 'w', mods = 'SUPER', action = act.CloseCurrentTab{ confirm = true } },
  -- { key = 'x', mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },
  -- { key = 'z', mods = 'SHIFT|CTRL', action = act.TogglePaneZoomState },
  -- { key = '{', mods = 'SUPER', action = act.ActivateTabRelative(-1) },
  -- { key = '{', mods = 'SHIFT|SUPER', action = act.ActivateTabRelative(-1) },
  -- { key = '|', mods = 'SHIFT|LEADER', action = act.SplitHorizontal{ domain =  'CurrentPaneDomain' } },
  -- { key = '}', mods = 'SUPER', action = act.ActivateTabRelative(1) },
  -- { key = '}', mods = 'SHIFT|SUPER', action = act.ActivateTabRelative(1) },
  -- { key = 'phys:Space', mods = 'SHIFT|CTRL', action = act.QuickSelect },
  -- { key = 'PageUp', mods = 'SHIFT', action = act.ScrollByPage(-1) },
  -- { key = 'PageUp', mods = 'CTRL', action = act.ActivateTabRelative(-1) },
  -- { key = 'PageUp', mods = 'SHIFT|CTRL', action = act.MoveTabRelative(-1) },
  -- { key = 'PageDown', mods = 'SHIFT', action = act.ScrollByPage(1) },
  -- { key = 'PageDown', mods = 'CTRL', action = act.ActivateTabRelative(1) },
  -- { key = 'PageDown', mods = 'SHIFT|CTRL', action = act.MoveTabRelative(1) },
  -- { key = 'LeftArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Left' },
  -- { key = 'LeftArrow', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize{ 'Left', 1 } },
  -- { key = 'RightArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Right' },
  -- { key = 'RightArrow', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize{ 'Right', 1 } },
  -- { key = 'UpArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Up' },
  -- { key = 'UpArrow', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize{ 'Up', 1 } },
  -- { key = 'DownArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Down' },
  -- { key = 'DownArrow', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize{ 'Down', 1 } },
  -- { key = 'Copy', mods = 'NONE', action = act.CopyTo 'Clipboard' },
  -- { key = 'Paste', mods = 'NONE', action = act.PasteFrom 'Clipboard' },
}


wezterm.on('format-tab-title', function(tab)
  -- Get the process name.
  local process = string.gsub(tab.active_pane.foreground_process_name, '(.*[/\\])(.*)', '%2')

  -- Current working directory.
  local cwd = tab.active_pane.current_working_dir
  cwd = cwd and string.format('%s ', cwd.file_path:gsub(os.getenv 'HOME', '~')) or ''

  -- Format and return the title.
  return string.format('(%d %s) %s', tab.tab_index + 1, process, cwd)
end)

-- wezterm.on('format-window-title', function(tab, pane, tabs, panes, config)
--   local zoomed = ''
--   if tab.active_pane.is_zoomed then
--     zoomed = '[Z] '
--   end
--
--   local index = ''
--   if #tabs > 1 then
--     index = string.format('[%d/%d] ', tab.tab_index + 1, #tabs)
--   end
--
-- --   cwd = cwd and string.format('%s ', cwd.file_path:gsub(os.getenv 'HOME', '~')) or ''
--   return zoomed .. index .. tab.active_pane.title
-- end)

-- and finally, return the configuration to wezterm
return config
