from typing import List
from random import random, randint
from time import sleep

JAMMING_SIGNAL = float('inf')


class Packet(object):
    def __init__(self, value=5, hp=3):
        self._value = value
        self._hp = hp
        self._bleeding = False

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, new_value):
        self._value = new_value

    @property
    def hp(self):
        return self._hp

    @hp.setter
    def hp(self, new_hp):
        self._hp = new_hp

    @property
    def bleeding(self):
        return self._bleeding

    def decrease_hp(self):
        self._hp -= 1
        self._bleeding = True

    def __add__(self, foreign):
        return Packet(value=self.value + foreign.value, hp=self.hp)


class Device(object):

    def __init__(self, ethernet_cable_pos):
        self._ethernet_cable_pos = ethernet_cable_pos
        self._exp_backoff_iterator = 0
        self._until_next_packet = 0
        self._currently_transmitted_value = 0
        self._jamming_mode = False
        self._exp_backoff = False

    @property
    def ethernet_cable_pos(self):
        return self._ethernet_cable_pos

    @property
    def exp_backoff_iterator(self):
        return self._exp_backoff_iterator

    @exp_backoff_iterator.setter
    def exp_backoff_iterator(self, new_val):
        self._exp_backoff_iterator = new_val

    @property
    def is_ready(self):
        if self.until_next_packet == 0 and self.currently_transmitted_value == 0 and not self.jamming_mode:
            return True
        else:
            return False

    def perform_exponential_backoff(self):
        k = 10 if self._exp_backoff_iterator > 10 else self._exp_backoff_iterator
        self._until_next_packet = randint(0, 2 ** k)
        self._exp_backoff = True

    @property
    def currently_transmitted_value(self):
        return self._currently_transmitted_value

    @currently_transmitted_value.setter
    def currently_transmitted_value(self, new_val):
        self._currently_transmitted_value = new_val

    @property
    def jamming_mode(self):
        return self._jamming_mode

    @jamming_mode.setter
    def jamming_mode(self, new_val):
        self._jamming_mode = new_val

    @property
    def until_next_packet(self):
        return self._until_next_packet

    @until_next_packet.setter
    def until_next_packet(self, new_val):
        self._until_next_packet = new_val

    @property
    def exp_backoff(self):
        return self._exp_backoff

    def reset_exp_backoff(self):
        self._exp_backoff = False
        self._exp_backoff_iterator = 0


def transmit(eth_cable: List[Packet]) -> ():
    i = 0
    while i < len(eth_cable):

        packet = eth_cable[i]

        if packet is not None:

            if packet.hp < 2:
                # kill the packet that has bled out
                eth_cable[i] = None

            if not packet.bleeding:

                if i > 0:
                    if eth_cable[i - 1] is None:
                        eth_cable[i - 1] = Packet(packet.value, hp=packet.hp)
                    elif eth_cable[i - 1].value != packet.value:
                        eth_cable[i - 1] = packet + eth_cable[i - 1]

                if i < len(eth_cable) - 1:
                    if eth_cable[i + 1] is None:
                        eth_cable[i + 1] = Packet(packet.value, hp=packet.hp)
                        i += 1
                    elif eth_cable[i + 1].value != packet.value:
                        eth_cable[i + 1] = packet + eth_cable[i + 1]
                        i += 1

            packet.decrease_hp()

        i += 1

    return eth_cable


def can_send_packet(ethernet_cable: List[Packet], index: int) -> bool:
    # Searching if there is traffic
    length = len(ethernet_cable)

    # Beginning of the cable
    if index == 0:
        if ethernet_cable[index] is None and ethernet_cable[index + 1] is None:
            return True
    # End of the cable
    elif index == length-1:
        if ethernet_cable[index] is None and ethernet_cable[index - 1] is None:
            return True
    # Middle of the cable
    else:
        if ethernet_cable[index-1] is None and ethernet_cable[index] is None and ethernet_cable[index + 1] is None:
            return True
    return False


if __name__ == '__main__':
    ethernet_cable = [None for _ in range(22)]
    devices = [
        Device(3),
        Device(8),
        Device(15)
    ]
    iterations = 500
    # probability of a device to emit some data
    probability = 0.001
    # packet length
    packet_length = len(ethernet_cable)

    ethernet_cable[3] = Packet(value=1, hp=packet_length)
    ethernet_cable[8] = Packet(value=2, hp=packet_length)
    ethernet_cable[15] = Packet(value=3, hp=packet_length)
    devices[0].currently_transmitted_value = 1
    devices[1].currently_transmitted_value = 2
    devices[2].currently_transmitted_value = 3

    data = 10
    for _ in range(1, iterations + 1):

        for device in devices:

            if device.exp_backoff_iterator > 15:
                # abort
                device.currently_transmitted_value = 0
                device.reset_exp_backoff()

            if device.is_ready and random() > 1-probability:
                device.currently_transmitted_value = data
                data += 1

            t = ethernet_cable[device.ethernet_cable_pos]

            if can_send_packet(ethernet_cable, device.ethernet_cable_pos):

                if device.until_next_packet == 0 and device.currently_transmitted_value != 0:
                    ethernet_cable[device.ethernet_cable_pos] = Packet(value=device.currently_transmitted_value,
                                                                       hp=packet_length)
                else:
                    device.until_next_packet -= 1

            # There is something going on near the device
            if t is not None and device.currently_transmitted_value != 0:
                # The packet has been successfully transferred
                if t.value == device.currently_transmitted_value and t.hp == 1:
                    device.currently_transmitted_value = 0
                    device.reset_exp_backoff()

                # Transmitting jamming signal
                elif t.value not in [JAMMING_SIGNAL, device.currently_transmitted_value] and device.until_next_packet < 1:
                    ethernet_cable[device.ethernet_cable_pos] = Packet(value=JAMMING_SIGNAL,
                                                                       hp=packet_length)
                    device.exp_backoff_iterator += 1
                    device.perform_exponential_backoff()

                # Jamming signal has been seen - wait random time
                if t.value == JAMMING_SIGNAL and device.until_next_packet == 0:
                    device.perform_exponential_backoff()

        transmit(ethernet_cable)
        sleep(0.1)
        for el in map(lambda x: str(x.hp).ljust(2) + ' ' + str(x.value if x.value != JAMMING_SIGNAL else '!!!').ljust(4) if x is not None else '       ', ethernet_cable):
            print(el, end=' ')
        print()
