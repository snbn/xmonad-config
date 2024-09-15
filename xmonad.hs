import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.ManageHook
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig

myConfig =
  def
    { modMask = mod4Mask,
      layoutHook = myLayout,
      manageHook = myManageHook,
      startupHook = myStartupHook
    }
    `removeKeysP` ["M-S-q"]
    `additionalKeysP` [ ("M-S-q", spawn "rofi -show p -modi p:'~/aur/rofi-power-menu/rofi-power-menu --choices logout/reboot/shutdown --symbols-font \"Symbols Nerd Font\"'")
                      ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- \| Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myManageHook :: ManageHook
myManageHook = (isDialog --> doFloat)

myStartupHook :: X ()
myStartupHook = do
  spawn "picom -b --log-level trace --log-file /tmp/picom.log"
