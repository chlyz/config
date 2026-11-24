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

-- Remove gaps when only one tiled window is visible.
hl.workspace_rule({ workspace = "w[tv1]", gaps_in = 0, gaps_out = 0 })
-- Hide the frame on a lone tiled window without affecting floating windows.
hl.workspace_rule({ workspace = "w[1] w[t1]", no_border = true })

local config_home = os.getenv("XDG_CONFIG_HOME") or os.getenv("HOME") .. "/.config"
local theme_path = config_home .. "/modus-theme/hyprland.lua"
local theme_file = io.open(theme_path, "r")
if theme_file then
    theme_file:close()
    dofile(theme_path)
end

-- The native layouts keep independent orders. Carry the master order through
-- monocle so zoom and focus cycling use the same list in both layouts.
local monocles = {}
local function layout(message)
    hl.dispatch(hl.dsp.layout(message))
end

local function tiles(workspace)
    local windows = {}
    for _, w in ipairs(hl.get_workspace_windows(workspace)) do
        if not w.floating and not w.hidden then windows[#windows + 1] = w end
    end
    return windows
end

local function master_order(workspace)
    local windows = tiles(workspace)
    -- Left-oriented master, then stack, each read from top to bottom.
    table.sort(windows, function(a, b)
        if a.at.x == b.at.x then return a.at.y < b.at.y end
        return a.at.x < b.at.x
    end)
    local order = {}
    for i, w in ipairs(windows) do order[i] = w.stable_id end
    return order
end

local function index_of(order, id)
    for i, entry in ipairs(order) do
        if entry == id then return i end
    end
end

local function monocle_order(workspace, state)
    local live, order = {}, {}
    for _, w in ipairs(tiles(workspace)) do
        live[w.stable_id] = true
        if not index_of(state.order, w.stable_id) then
            table.insert(order, 1, w.stable_id)
        end
    end
    for _, id in ipairs(state.order) do
        if live[id] then order[#order + 1] = id end
    end
    state.order = order
    return order
end

local function show_monocle(id, count)
    -- Inactive monocle windows block direct focus. Cycle the native layout.
    for _ = 1, count do
        local w = hl.get_active_window()
        if w and w.stable_id == id then return end
        layout("cyclenext")
    end
end

local function focus_id(id)
    hl.dispatch(hl.dsp.focus({ window = "stableid:" .. id }))
end

local function toggle_monocle()
    local ws = hl.get_active_workspace()
    if not ws or ws.id < 1 or hl.get_active_special_workspace() then return end
    local w = hl.get_active_window()
    local focused = w and w.workspace and w.workspace.id == ws.id and w.stable_id
    local state = monocles[ws.id] or {}
    monocles[ws.id] = state
    if ws.tiled_layout == "master" then
        state.order = master_order(ws)
        if state.rule then
            state.rule:set_enabled(true)
        else
            state.rule = hl.workspace_rule({
                workspace = tostring(ws.id), layout = "monocle", no_border = true,
            })
        end
        hl.exec_scheduled_prop_refresh_immediately()
        if index_of(state.order, focused) then
            show_monocle(focused, #state.order)
        elseif focused then
            focus_id(focused)
        end
    elseif ws.tiled_layout == "monocle" and state.rule then
        local wanted = monocle_order(ws, state)
        state.rule:set_enabled(false)
        hl.exec_scheduled_prop_refresh_immediately()
        if ws.tiled_layout ~= "master" then return end
        local order = master_order(ws)
        for i, id in ipairs(wanted) do
            local pos = index_of(order, id)
            if pos and pos > i then
                focus_id(id)
                for _ = i + 1, pos do layout("swapprev noloop") end
                table.insert(order, i, table.remove(order, pos))
            end
        end
        if focused then focus_id(focused) end
    end
end

local function cycle(step)
    local ws = hl.get_active_special_workspace() or hl.get_active_workspace()
    local state = ws and monocles[ws.id]
    if ws and ws.tiled_layout == "monocle" and state and state.order then
        local order = monocle_order(ws, state)
        local w = hl.get_active_window()
        local i = w and index_of(order, w.stable_id)
        if i then
            show_monocle(order[(i - 1 + step) % #order + 1], #order)
            return
        end
    end
    layout(step == 1 and "cyclenext" or "cycleprev")
end

local function zoom()
    local w = hl.get_active_window()
    if not w or w.floating or not w.workspace then return end
    local ws = w.workspace
    local state = monocles[ws.id]
    local order
    if ws.tiled_layout == "master" then
        order = master_order(ws)
    elseif ws.tiled_layout == "monocle" and state and state.order then
        order = monocle_order(ws, state)
    else
        return
    end
    local i = index_of(order, w.stable_id)
    if not i or #order < 2 then return end
    if i == 1 then i = 2 end
    if ws.tiled_layout == "master" then
        focus_id(order[i])
        for _ = 2, i do layout("swapprev noloop") end
    else
        show_monocle(order[i], #order)
        table.insert(order, 1, table.remove(order, i))
    end
end

hl.bind("ALT + comma", function() cycle(-1) end)
hl.bind("ALT + period", function() cycle(1) end)
hl.bind("ALT + E", zoom)
hl.bind("SUPER + E", zoom)
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

hl.bind("SUPER + Return", hl.dsp.exec_cmd("kitty"))
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
