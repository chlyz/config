-- dwm-style master/stack, with the existing Dvorak and Alt shortcuts.
-- Hyprland 0.57.2. Workspaces approximate tags; combined tag views differ.
hl.monitor({
    output = "", mode = "preferred", position = "auto", scale = 2,
    bitdepth = 10,
})

hl.config({
    -- Avoid TV HDR mode switches when fullscreen video gains or loses focus.
    render = { cm_auto_hdr = 0 },
    general = {
        layout = "master",
        -- Hyprland applies inner gaps on both sides of adjacent windows.
        -- Match the outer gap to the combined inner gap between windows.
        gaps_in = 3,
        gaps_out = 6,
        border_size = 1,
        col = {
            active_border = "rgb(005e8b)",
            inactive_border = "rgb(9f9690)",
        },
    },
    master = {
        mfact = 0.55,
        new_status = "master",
        new_on_top = true,
        orientation = "left",
    },
    decoration = {
        rounding = 0,
        shadow = { enabled = false },
        blur = { enabled = false },
    },
    animations = { enabled = false },
    misc = { disable_hyprland_logo = true, disable_splash_rendering = true },
    input = {
        kb_layout = "custom,se,custom,custom",
        kb_variant = "dvorak_shift,,colemak_shift,colemak_dh_shift",
        kb_options = "ctrl:swap_lalt_lctl,ctrl:swap_ralt_rctl,caps:swapescape",
        follow_mouse = 1,
    },
})

-- Match Sway's smart gaps and borders for a lone visible tiled window.
hl.workspace_rule({ workspace = "w[tv1]", gaps_in = 0, gaps_out = 0 })
hl.window_rule({
    name = "smart-borders",
    match = { float = false, workspace = "w[tv1]" }, border_size = 0,
})

local config_home = os.getenv("XDG_CONFIG_HOME") or os.getenv("HOME") .. "/.config"
local theme_path = config_home .. "/modus-theme/hyprland.lua"
local theme_file = io.open(theme_path, "r")
if theme_file then
    theme_file:close()
    dofile(theme_path)
end

-- Native monocle on the focused workspace, leaving other workspaces alone.
local monocles = {}
local function toggle_monocle()
    local workspace = hl.get_active_workspace()
    if not workspace or not workspace.id or workspace.id < 1 then return end
    local id = workspace.id
    if monocles[id] then
        monocles[id]:set_enabled(not monocles[id]:is_enabled())
    else
        monocles[id] = hl.workspace_rule({
            workspace = tostring(id), layout = "monocle",
        })
    end
end

hl.bind("ALT + comma", hl.dsp.layout("cycleprev"))
hl.bind("ALT + period", hl.dsp.layout("cyclenext"))
hl.bind("ALT + E", hl.dsp.layout("swapwithmaster master"))
hl.bind("ALT + U", toggle_monocle)
hl.bind("ALT + O", hl.dsp.focus({ workspace = "previous" }))
hl.bind("ALT + H", hl.dsp.layout("mfact -0.05"), { repeating = true })
hl.bind("ALT + L", hl.dsp.layout("mfact +0.05"), { repeating = true })
hl.bind("ALT + I", hl.dsp.layout("addmaster"))
hl.bind("ALT + D", hl.dsp.layout("removemaster"))
hl.bind("ALT + SHIFT + space", hl.dsp.window.float({ action = "toggle" }))
hl.bind("ALT + B", hl.dsp.exec_cmd("pkill -USR1 -x waybar"))
hl.bind("ALT + P", hl.dsp.exec_cmd("rofi -show drun"))
for i = 1, 9 do
    hl.bind("ALT + " .. i, hl.dsp.focus({ workspace = i }))
    hl.bind("ALT + SHIFT + " .. i,
        hl.dsp.window.move({ workspace = i, follow = false }))
end

-- Match Sway's workspace shortcuts, with 0 selecting workspace 10.
for i = 1, 10 do
    local key = tostring(i % 10)
    hl.bind("SUPER + " .. key, hl.dsp.focus({ workspace = i }))
    hl.bind("SUPER + SHIFT + " .. key,
        hl.dsp.window.move({ workspace = i, follow = false }))
end

hl.bind("SUPER + Return", hl.dsp.exec_cmd("foot"))
hl.bind("SUPER + C", hl.dsp.window.close())
hl.bind("SUPER + SHIFT + R", hl.dsp.exec_cmd("hyprctl reload"))
hl.bind("SUPER + SHIFT + E", hl.dsp.exec_cmd(
    [[swaynag -t warning -m 'Exit Hyprland? This will end your Wayland session.' -B 'Yes, exit Hyprland' "hyprctl dispatch 'hl.dsp.exit()'"]]))
hl.bind("SUPER + F", hl.dsp.window.fullscreen({ mode = "fullscreen" }))
hl.bind("SUPER + T", hl.dsp.exec_cmd("~/.local/bin/toggle-terminal"))
hl.bind("SUPER + H", hl.dsp.exec_cmd("~/.local/bin/focus-emacs"))
hl.bind("SUPER + N", hl.dsp.exec_cmd("~/.local/bin/network-menu"))
hl.bind("SUPER + space", hl.dsp.exec_cmd("hyprctl switchxkblayout all next"))
hl.bind("SUPER + SHIFT + L", hl.dsp.exec_cmd("hyprlock"))
hl.bind("SUPER + SHIFT + G", hl.dsp.exec_cmd("~/.local/bin/hypr-game-mode"))
hl.bind("Print", hl.dsp.exec_cmd("grim"))
hl.bind("SUPER + S", hl.dsp.exec_cmd(
    [[region=$(slurp) && grim -g "$region" - | wl-copy]]))
hl.bind("ALT + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind("ALT + mouse:273", hl.dsp.window.resize(), { mouse = true })

local audio = {
    XF86AudioRaiseVolume = "wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+",
    XF86AudioLowerVolume = "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-",
    XF86AudioMute = "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle",
    XF86AudioMicMute = "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle",
}
for key, command in pairs(audio) do
    hl.bind(key, hl.dsp.exec_cmd(command), { locked = true, repeating = true })
end

local media = {
    XF86AudioPlay = "playerctl play-pause",
    XF86AudioPause = "playerctl play-pause",
    XF86AudioPrev = "playerctl previous",
    XF86AudioNext = "playerctl next",
    XF86AudioStop = "playerctl stop",
    XF86MonBrightnessDown = "brightnessctl set 5%-",
    XF86MonBrightnessUp = "brightnessctl set 5%+",
}
for key, command in pairs(media) do
    hl.bind(key, hl.dsp.exec_cmd(command), { locked = true })
end

hl.window_rule({
    name = "emacs-workspace", match = { class = "[Ee]macs" }, workspace = "1",
})
hl.window_rule({
    name = "dropdown", match = { class = "dropdown-terminal" },
    float = true, workspace = "special:dropdown", center = true,
})
-- Hyprland has no native "visible" idle rule. Enable the Steam inhibitor
-- while any Steam game is visible, including when another window has focus.
local steam_idle = hl.window_rule({
    name = "steam-idle", match = { class = "steam_app_[0-9]+" },
    idle_inhibit = "always", enabled = false,
})
local steam_visible = false
local function update_steam_idle()
    local visible = false
    for _, window in ipairs(hl.get_windows()) do
        if window.visible and (window.class or ""):match("^steam_app_%d+$") then
            visible = true
            break
        end
    end
    if visible ~= steam_visible then
        steam_idle:set_enabled(visible)
        steam_visible = visible
    end
end

local steam_timer
local function start_steam_monitor()
    if not steam_timer then
        steam_timer = hl.timer(update_steam_idle, { timeout = 1000, type = "repeat" })
        update_steam_idle()
    end
end
hl.on("config.reloaded", function()
    -- Verification has no active workspace or compositor event loop.
    if hl.get_active_workspace() then start_steam_monitor() end
end)

hl.on("hyprland.start", function()
    start_steam_monitor()
    hl.exec_cmd("dbus-update-activation-environment --systemd " ..
        "WAYLAND_DISPLAY XDG_CURRENT_DESKTOP HYPRLAND_INSTANCE_SIGNATURE " ..
        "GNOME_KEYRING_CONTROL SSH_AUTH_SOCK; " ..
        "systemctl --user start hypridle.service")
    hl.exec_cmd("nm-applet --indicator")
    hl.exec_cmd("/usr/libexec/lxqt-policykit-agent")
end)
