# TODO: Colorizar el numpad si esta activo
# TODO: Advertir de cuando esta la mayuscula activada
# TODO: Buscar una configuración predeterminada
# TODO: Añadir modos para vim

import colorsys
import random
from evdev import InputDevice, ecodes

from openrazer.client import DeviceManager
# from openrazer.client import constants as razer_constants

# Create a DeviceManager. This is used to get specific devices
device_manager = DeviceManager()


print("Found {} Razer devices".format(len(device_manager.devices)))
print()

# Disable daemon effect syncing.
# Without this, the daemon will try to set the lighting effect to every device.
device_manager.sync_effects = False


# Helper funciton to generate interesting colors
def random_color():
    rgb = colorsys.hsv_to_rgb(
        random.uniform(0.7, 0.75), random.uniform(0.8, 1), 1)
    return tuple(map(lambda x: int(256 * x), rgb))


class KeyboardConfig(object):

    """Set config fo """

    def __init__(self, device, keyboard_input):
        self.device = device
        self.keyboard_input = keyboard_input
        self.rows = device.fx.advanced.rows
        self.cols = device.fx.advanced.cols
        self.numpad_enable = 0 in keyboard_input.leds()
        self.caps_locks_enable = 1 in keyboard_input.leds()
        self.update_lights()

    def update_lights(self):
        self.normal_state()
        if self.numpad_enable:
            self.numpad_color(*random_color())
        else:
            self.numpad_color(0, 0, 0)

        if self.caps_locks_enable:
            self.caps_locks(0, 0, 0)

        self.device.fx.advanced.draw()

    def normal_state(self):
        for row in range(self.rows):
            for col in range(self.cols - 4):
                self.device.fx.advanced.matrix[row, col] = random_color()

    def numpad_color(self, *color):
        for row in range(self.rows):
            for col in range(self.cols - 4, self.cols):
                self.device.fx.advanced.matrix[row, col] = color

    def caps_locks(self, *color):
        for row in range(2, self.rows - 1):
            for col in range(2, self.cols - 8):
                self.device.fx.advanced.matrix[row, col] = color

    def listening(self):
        for event in self.keyboard_input.read_loop():
            if event.type == ecodes.EV_LED and event.code == 1:
                self.caps_locks_enable = bool(event.value)
            elif event.type == ecodes.EV_LED and event.code == 0:
                self.numpad_enable = bool(event.value)

            self.update_lights()


dev = InputDevice('/dev/input/by-id/usb-Razer_Razer_Ornata_Chroma-event-kbd')

config = KeyboardConfig(device_manager.devices[0], dev)

config.listening()
