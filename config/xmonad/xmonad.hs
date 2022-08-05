import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

import XMonad.Hooks.StatusBar
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops


main :: IO ()
main = xmonad
     . withEasySB (statusBarProp "xmobar" $ pure def) defToggleStrutsKey
     . ewmh
     $ xmonadConfig

xmonadConfig = def { modMask         = mod1Mask
                   , handleEventHook = fullscreenEventHook
                   , terminal        = "termonad"
                   }
               `additionalKeysP`
               [ ("M-]"         , spawn "firefox --private-window")
               , ("M-y"         , unGrab >> spawn "screenshot"    )
               , ("M--"         , spawn "amixer sset Master 5%-"  )
               , ("M-="         , spawn "amixer sset Master 5%+"  )
               , ("M-p"         , spawn "rofi -show run"          )
               ]
