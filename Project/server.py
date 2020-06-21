import asyncio
import argparse
import time
import sys
import logging
import aiohttp
import json

PRINT_FLAG = 0
LOG_FLAG = 1
KEY = 'AIzaSyAAE36Qo4tI0aWY1rVWNibmuiGoFNmTCyU'

server_names = {"Hill": 12090, "Jaquez": 12091, "Smith": 12092, "Campbell": 12093, "Singleton": 12094}
server_neighbors = {
    "Hill": ["Jaquez", "Smith"],
    "Jaquez": ["Hill", "Singleton"],
    "Smith": ["Hill", "Singleton", "Campbell"],
    "Campbell": ["Smith", "Singleton"],
    "Singleton": ["Campbell", "Jaquez", "Smith"]
}
localhost = '127.0.0.1'


class Server:
    def __init__(self, name, ip, port):
        self.name = name
        self.ip = ip
        self.port = port
        self.client_info = dict() # stores timestamp of when last message from client is received
        self.client_message = dict()
        logging.info("Initialize log file for server {}".format(name))


    async def handle_input(self, reader, writer):
        while not reader.at_eof():  # only exits when buffer is non-empty
            data = await reader.readline()
            message = data.decode()
            if message == "": # ignore empty messages
                continue
            print_log("{} recieved: {}".format(self.name, message))
            strings = message.split()
            # check for length of strings first
            if len(strings) != 4:  # not a regular command
                # check if its message from other servers
                if len(strings) == 6 and strings[0] == "AT":
                    # message propagated from other servers
                    print_log("Received propagated message...")
                    sendback_message = None
                    if strings[3] in self.client_info:
                        # already received message from same ID before
                        print_log("Already contain data for client {}".format(strings[3]))
                        if float(strings[5]) > self.client_info[strings[3]]: 
                            # update and propagate
                            print_log("Updating data for client {}... and propagating new data...".format(strings[1]))
                            self.client_info[strings[3]] = float(strings[5])
                            self.client_message[strings[3]] = message
                            await self.propagate_message(message)
                        else:
                            # already received and don't propagate
                            print_log("Received message already... Stop propagation.")
                            pass
                    else: 
                        # have not received message from this ID
                        print_log("New data for client {}... propagate new data...".format(strings[1]))
                        self.client_info[strings[3]] = float(strings[5])
                        self.client_message[strings[3]] = message
                        await self.propagate_message(message)
                else: 
                    # invalid message
                    sendback_message = "? " + message
            elif strings[0] == "IAMAT":
                if self.validIAMAT(strings):
                    # prepare difference in timestamp
                    diff = time.time() - float(strings[3])
                    # appends + in front of non-zero positive numbers
                    str_diff = ["", "+"][diff > 0] + str(diff)
                    sendback_message = "AT {} {} {} {} {}".format(self.name, str_diff, strings[1], strings[2], strings[3])
                    # update client ID and timestamp in client_info
                    self.client_info[strings[1]] = float(strings[3])
                    # store current message in client_message
                    self.client_message[strings[1]] = sendback_message
                    # propagate sendback_message to other servers
                    await self.propagate_message(sendback_message)
                else:
                    sendback_message = "? " + message
            elif strings[0] == "WHATSAT":
                if self.validWHATSAT(strings):
                    # get location of client
                    location = self.client_message[strings[1]].split()[4]
                    radius = strings[2]
                    bound = strings[3]
                    places = await self.get_places(location, radius, bound)
                    sendback_message = "{}\n{}\n\n".format(self.client_message[strings[1]], str(places).rstrip('\n'))
                else:
                    sendback_message = "? " + message
            else:
                sendback_messgae = "? " + message

            # send proper message back to client
            if sendback_message != None:
                print_log("Sending message back to client: {}".format(sendback_message))
                writer.write(sendback_message.encode())
                await writer.drain()

        print_log("close the client socket")
        writer.close()


    async def propagate_message(self, message):
        # send message to every server connected
        for neighbor in server_neighbors[self.name]:
            try:
                reader, writer = await asyncio.open_connection('127.0.0.1', server_names[neighbor])
                print_log("{} send to {}: {}".format(self.name, neighbor, message))
                writer.write(message.encode())
                await writer.drain()
                print_log("Closing connection to {}".format(neighbor))
                writer.close()
                await writer.wait_closed()
            except:
                print_log("Error connecting to server {}".format(neighbor))


    async def get_places(self, location, radius, upper_bound):
        async with aiohttp.ClientSession() as session:
            # need to construct arguments for api
            coordinates = self.get_coordinates(location)
            if coordinates == None:  # should not happen
                print("False coordinate format")
                sys.exit()
            print_log("Attempting to retrieve places at location {}".format(coordinates))
            url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={}&radius={}&key={}'.format(coordinates, radius, KEY)
            result = await self.fetch(session, url)
            result_object = json.loads(result)
            print_log("Successfully retrieved {} place(s) from API".format(len(result_object["results"])))
            if len(result_object["results"]) <= int(upper_bound):
                return result
            else:
                # need to filter out extra results from result object
                result_object["results"] = result_object["results"][0:int(upper_bound)]
                return json.dumps(result_object, sort_keys=True, indent=4)
             

    async def fetch(self, session, url):
        async with session.get(url) as response:
            return await response.text()


    # need to add comma before second -, +
    def get_coordinates(self, location):
        plus = location.rfind('+')
        minus = location.rfind('-')
        if plus != -1 and plus != 0:
            return "{},{}".format(location[0:plus], location[plus:])
        if minus != -1 and minus != 0:
            return "{},{}".format(location[0:minus], location[minus:])
        return None


    def validIAMAT(self, strings):
        # check 3rd argument valid ISO 6709 notation
        temp = strings[2].replace('+', '-')
        nums = list(filter(None, temp.split('-')))
        if len(nums) != 2 or not (is_number(nums[0]) and is_number(nums[1])):
            return False
        # check 4th argument float
        if not is_number(strings[3]):
            return False
        return True
    
    
    def validWHATSAT(self, strings):
        # check third and fourth argument valid numbers
        if not (is_number(strings[2]) and is_number(strings[3])):
            return False
        # check radius within 50 
        if int(strings[2]) > 50 or int(strings[2]) < 0:
            return False
        # check information bound at most 20 items
        if int(strings[3]) > 20 or int(strings[3]) < 0:
            return False
        # check if current client exists
        if strings[1] not in self.client_info:
            return False
        return True


    async def run_forever(self):
        print_log('starting up {} server...'.format(self.name))
        # start up server
        server = await asyncio.start_server(self.handle_input, self.ip, self.port)

        # Serve requests until Ctrl+C is pressed
        async with server:
            await server.serve_forever()
        
        print_log('{} server shutting down...'.format(self.name))
        # Close the server
        server.close()


def is_number(string):
    try:
        float(string)
        return True
    except ValueError:
        return False


def print_log(msg):
    if LOG_FLAG:
        logging.info(msg)
    if PRINT_FLAG:
        print(msg)


def main():
    parser = argparse.ArgumentParser('CS131 Project Argument Parser')
    parser.add_argument('server_name', type=str, help='required server name input')
    args = parser.parse_args()
    if not args.server_name in server_names:
        print("Invalid Server Name {}".format(args.server_name))
        sys.exit()
    logging.basicConfig(filename="server_{}.log".format(args.server_name), format='%(levelname)s: %(message)s', filemode='w+', level=logging.INFO)
    server = Server(args.server_name, localhost, server_names[args.server_name])
    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    main()