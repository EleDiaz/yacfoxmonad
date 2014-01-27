yacfoxmonad
===========

Yet another config file of Xmonad. Programs used for this configuration:

 * xmonad, xmonad-contrib
 * xmobar
 * Kashe - Pequeño programa artesanal, para no tener que usar trayicon..., siver para ver la hora y los iconos de notificación
   * Su compilación debe ser tal que así(sino, no funcionará la hora!):
     ```shell
     ghc Kashe.hs -threaded
     ```

```haskell
main :: IO ()
main = do

  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ defaultConfig
    { terminal           = myTerminal
    -- ^ Elige tu terminal
    , focusFollowsMouse  = False
    -- ^ El raton cambia el foco segun lo mueves de una a otra ventana
    , borderWidth        = 0
    -- ^ Un borde para separar las ventanas
    , modMask            = myModMask
    -- ^ Elige tu tecla 'maestra'
    , workspaces         = myWorkspaces
    -- ^ Escritorios disponibles (una lista de cadenas)
    , logHook            = myLogHook
    -- ^ Para poder saber en que ventana, escritorio... estoy.
    , layoutHook         = showWName myLayouts
    -- ^ Configuración del posicionamiento de las ventanas
    , manageHook         = myManageHook
                           <+> manageDocks
                           <+> dynamicMasterHook
                           <+> toggleHook "float" doFloat
                           <+> namedScratchpadManageHook scratchpads
    -- ^ Se controla donde se ponen las ventanas, su comportamiento según escritorio
    , handleEventHook    = myHandleEventHook
    , startupHook        = myStartupHook >> checkKeymap defaultConfig myKeys
    -- ^ Cuando iniciamos el Tiling manager, importante chequear las teclas, para evitar colisiones
    }
     `additionalKeysP` myKeys
```

Preview
-------

La imagen no coincide exactamente pero viene siendo algo parecido, con fondo distinto
![Image](../master/preview.png?raw=true)
