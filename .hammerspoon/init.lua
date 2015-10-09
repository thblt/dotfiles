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
+---|---|-- Maximize current window
    +---|-- Place current window on left half of the screen
        +-- Place current window on right half of the screen
G4  G5  G6
+---|---|-- Place current window on left third of the screen
    +---|-- Place current window on center third of the screen
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
+---|---|--
    +---|--
        +--
--]]

gKeyPressedBindings[1] = function()
	local win = hs.window.focusedWindow()
	local f = win:frame()
	local max = win:screen():frame()

	f.x = max.x
	f.y = max.y
	f.w = max.w / 2
	f.h = max.h
	win:setFrame(f)
end

gKeyPressedBindings[2] = function()
	local win = hs.window.focusedWindow()
	local f = win:frame()
	local max = win:screen():frame()

	f.x = max.x + max.w / 2
	f.y = max.y
	f.w = max.w / 2
	f.h = max.h
	win:setFrame(f)
end

gKeyPressedBindings[3] = function()
	local win = hs.window.focusedWindow()
	local f = win:frame()
	local max = win:screen():frame()

	f.x = max.x + (.05 * max.w)
	f.y = max.y + (.05 * max.h)
	f.w = max.w * .9
	f.h = max.h * .9
	win:setFrame(f)
end

hs.notify.show("Hammerspoon", "", "Configuration file loaded", "sdds")
