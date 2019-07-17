# TODO: Colorizar el numpad si esta activo
# TODO: Advertir de cuando esta la mayuscula activada
# TODO: Buscar una configuración predeterminada
# TODO: Añadir modos para vim

import colorsys
import random
import asyncio
from evdev import InputDevice, ecodes, util, events

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
class Effect:
    pass


class KeyboardConfig(object):

    """Set config fo """

    def __init__(self, device, keyboard_input, keyboard_input_1):
        self.saturation = 100
        self.actual_color = 120 # random.uniform(0, 255)
        self.increment = 0
        self.device = device
        self.keyboard_input = keyboard_input
        self.keyboard_input_1 = keyboard_input_1
        self.rows = device.fx.advanced.rows
        self.cols = device.fx.advanced.cols
        self.numpad_enable = 0 in keyboard_input_1.leds()
        self.caps_locks_enable = 1 in keyboard_input_1.leds()
        self.generate_color()
        self.update_lights()

    def generate_color(self):
        # self.actual_color = (self.actual_color + self.increment) % 255
        next = self.actual_color + 100
        actual_color = random.uniform(self.actual_color, next) % 255
        rgb = colorsys.hsv_to_rgb(actual_color / 255, self.saturation / 100, 1)
        self.color_rgb = list(map(lambda x: int(255 * x), rgb))

    def update_saturation(self):
        rgb = colorsys.hsv_to_rgb(self.actual_color / 255, self.saturation / 100, 1)
        self.color_rgb = list(map(lambda x: int(255 * x), rgb))

    def update_lights(self):
        color = self.color_rgb
        self.normal_state(color)
        if not self.numpad_enable:
            self.numpad_color(0, 0, 0)

        if self.caps_locks_enable:
            self.caps_locks(0, 0, 0)

        self.device.fx.advanced.draw()

    def normal_state(self, color):
        for row in range(self.rows):
            for col in range(self.cols):
                self.generate_color()
                color = self.color_rgb
                self.device.fx.advanced.matrix[row, col] = color

    def numpad_color(self, *color):
        for row in range(self.rows):
            for col in range(self.cols - 4, self.cols):
                self.device.fx.advanced.matrix[row, col] = color

    def caps_locks(self, *color):
        for row in range(2, self.rows - 1):
            for col in range(2, self.cols - 8):
                self.device.fx.advanced.matrix[row, col] = color

    async def process(self, device):
        async for event in device.async_read_loop():
            if event.type == ecodes.EV_LED and event.code == 1:
                self.caps_locks_enable = bool(event.value)
                self.update_lights()
            elif event.type == ecodes.EV_LED and event.code == 0:
                self.numpad_enable = bool(event.value)
                self.update_lights()
            else:
                key = util.categorize(event)
                if isinstance(key, events.KeyEvent) and key.keystate == 1:
                    pass
                    # self.saturation -= 5
                    # if self.saturation < 0:
                    #     self.saturation = 0
                    # self.generate_color()
                    # self.update_lights()

    async def increase_saturation(self):
        while True:
            self.saturation += 5
            if self.saturation > 100:
                self.saturation = 100
            self.update_saturation()
            self.update_lights()
            await asyncio.sleep(0.7)

    def listening(self):
        for device in self.keyboard_input, self.keyboard_input_1:
            asyncio.ensure_future(self.process(device))

        loop = asyncio.get_event_loop()
        # asyncio.ensure_future(self.increase_saturation())
        loop.run_forever()


dev = InputDevice('/dev/input/by-id/usb-Razer_Razer_Ornata_Chroma-event-kbd')
dev1 = InputDevice('/dev/input/by-id/usb-Razer_Razer_Ornata_Chroma-if01-event-kbd')

config = KeyboardConfig(device_manager.devices[0], dev1, dev)

config.listening()
