module CSS.MediaQuery where

import Prelude2
import qualified Data.Text.Lazy as TL

data Expr = Expr TL.Text deriving Show

{-
all 	Used for all media type devices
print 	Used for printers
screen 	Used for computer screens, tablets, smart-phones etc.
speech 	Used for screenreaders that "reads" the page out loud
aural 	Deprecated. Used for speech and sound synthesizers
braille 	Deprecated. Used for braille tactile feedback devices
embossed 	Deprecated. Used for paged braille printers
handheld 	Deprecated. Used for small or handheld devices
projection 	Deprecated. Used for projected presentations, like slides
tty 	Deprecated. Used for media using a fixed-pitch character grid, like teletypes and terminals
tv 	Deprecated. Used for television-type devices

any-hover 	Does any available input mechanism allow the user to hover over elements? (added in Media Queries Level 4)
any-pointer 	Is any available input mechanism a pointing device, and if so, how accurate is it? (added in Media Queries Level 4)
aspect-ratio 	The ratio between the width and the height of the viewport
color 	The number of bits per color component for the output device
color-index 	The number of colors the device can display
device-aspect-ratio 	Deprecated. The ratio between the width and the height of the device
device-height 	Deprecated. The height of the device, such as a computer screen
device-width 	Deprecated. The width of the device, such as a computer screen
grid 	Whether the device is a grid or bitmap
height 	The viewport height
hover 	Does the primary input mechanism allow the user to hover over elements? (added in Media Queries Level 4)
inverted-colors 	Is the browser or underlying OS inverting colors? (added in Media Queries Level 4)
light-level 	Current ambient light level (added in Media Queries Level 4)
max-aspect-ratio 	The maximum ratio between the width and the height of the display area
max-color 	The maximum number of bits per color component for the output device
max-color-index 	The maximum number of colors the device can display
max-device-aspect-ratio 	The maximum ratio between the width and the height of the device
max-device-height 	The maximum height of the device, such as a computer screen
max-device-width 	The maximum width of the device, such as a computer screen
max-height 	The maximum height of the display area, such as a browser window
max-monochrome 	The maximum number of bits per "color" on a monochrome (greyscale) device
max-resolution 	The maximum resolution of the device, using dpi or dpcm
max-width 	The maximum width of the display area, such as a browser window
min-aspect-ratio 	The minimum ratio between the width and the height of the display area
min-color 	The minimum number of bits per color component for the output device
min-color-index 	The minimum number of colors the device can display
min-device-aspect-ratio 	The minimum ratio between the width and the height of the device
min-device-width 	The minimum width of the device, such as a computer screen
min-device-height 	The minimum height of the device, such as a computer screen
min-height 	The minimum height of the display area, such as a browser window
min-monochrome 	The minimum number of bits per "color" on a monochrome (greyscale) device
min-resolution 	The minimum resolution of the device, using dpi or dpcm
min-width 	The minimum width of the display area, such as a browser window
monochrome 	The number of bits per "color" on a monochrome (greyscale) device
orientation 	The orientation of the viewport (landscape or portrait mode)
overflow-block 	How does the output device handle content that overflows the viewport along the block axis (added in Media Queries Level 4)
overflow-inline 	Can content that overflows the viewport along the inline axis be scrolled (added in Media Queries Level 4)
pointer 	Is the primary input mechanism a pointing device, and if so, how accurate is it? (added in Media Queries Level 4)
resolution 	The resolution of the output device, using dpi or dpcm
scan 	The scanning process of the output device
scripting 	Is scripting (e.g. JavaScript) available? (added in Media Queries Level 4)
update-frequency 	How quickly can the output device modify the appearance of the content (added in Media Queries Level 4)
width 	The viewport width
-}
