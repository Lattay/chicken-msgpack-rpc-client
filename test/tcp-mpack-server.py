import asyncio
import sys
import aio_msgpack_rpc


try:
    port = int(sys.argv[1])
except Exception:
    port = 8888


# handlers can be defined on a class
# they can either be async or plain functions
class MyServicer:
    async def sum(self, x, y):
        # print(f"sum: {x}, {y}")
        return x + y

    async def prod(self, x, y):
        return x * y

    async def answer(self):
        return 42

    async def i_dont_know(self, a, b, c):
        return None

    async def call_me_back(self, name):
        return None

    async def dont_call_me(self):
        raise Exception('Don\'t call me !')


async def main():
    try:
        server = await asyncio.start_server(
            aio_msgpack_rpc.Server(MyServicer()),
            host="127.0.0.1",
            port=port
        )

        while True:
            await asyncio.sleep(0.1)
    finally:
        server.close()

try:
    asyncio.get_event_loop().run_until_complete(main())
except KeyboardInterrupt:
    pass
