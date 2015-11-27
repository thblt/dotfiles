-- Personal settings. I have changing taste.

termEmulatorName = "iTerm"
textEditorName = "Emacs"
webBrowserName = "Chromium"


-- Variables

hs.window.animationDuration = 0 -- No animation

-- Automate config reload

function reloadConfig(files)
	doReload = false
	for _,file in pairs(files) do
		if file:sub(-4) == ".lua" then
			doReload = true
		end
	end
	if doReload then
		hs.notify.show("Hammerspoon", "", "Reloading on configuration change", "Details")
		hs.reload()
	end
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()

--- Handle Corsair keyboard G-Keys

gKeyPressedBindings = {}
gKeyReleasedBindings = {}

hs.urlevent.bind("CorsairKbGKeyPressed", function(eventName, params)
	key = tonumber(params["key"])
	if gKeyPressedBindings[key] ~= nil then
		gKeyPressedBindings[key]()
	end
end)

hs.urlevent.bind("CorsairKbGKeyReleased", function(eventName, params)
	key = tonumber(params["key"])
	if gKeyReleasedBindings[key] ~= nil then
		gKeyReleasedBindings[key]()
	end
end)

--[[ G-Key Bindings
G1  G2  G3
+---|---|-- Place current window on left half of the screen
    +---|-- Place current window on right half of the screen
        +-- Maximize current window
G4  G5  G6
+---|---|-- Place current window on left of the screen, two thirds width.
    +---|-- Place current window on right of the screen, one third width.
        +-- Place current window on right third of the screen

G7  G8  G9
+---|---|--
    +---|--
        +--
G10 G11 G12
+---|---|--
    +---|--
        +--
 
G13 G14 G15
+---|---|--
    +---|--
        +--
G16 G17 G18
+---|---|-- Bring terminal to the front
    +---|-- Bring text editor to the front
        +-- Bring web browser to the front
--]]

function positionRelative(x, y, w, h)
	local win = hs.window.focusedWindow()
	local f = win:frame()
	local max = win:screen():frame()

	f.x = max.w * x
	f.y = max.h * y
	f.w = max.w * w
	f.h = max.h * h
	win:setFrame(f)
end

gKeyPressedBindings[1] = function()
	positionRelative(0, 0, 1/2, 1)
end

gKeyPressedBindings[2] = function()
	positionRelative(0,0,1,1)
end

gKeyPressedBindings[3] = function()
	positionRelative(1/2, 0, 1/2, 1)
end

gKeyPressedBindings[4] = function()
	positionRelative(0, 0, 2/3, 1)
end

gKeyPressedBindings[5] = function()
	positionRelative(2/3, 0, 1/3, 1)
end

gKeyPressedBindings[6] = function()
	positionRelative(2/3, 0, 1/3, 1)
end

gKeyPressedBindings[16] = function()
	hs.application.launchOrFocus(termEmulatorName)
end

gKeyPressedBindings[17] = function()
	hs.application.launchOrFocus(textEditorName)
end

gKeyPressedBindings[18] = function()
	hs.application.launchOrFocus(webBrowserName)
end

hs.notify.show("Hammerspoon", "", "Configuration file loaded", "")
