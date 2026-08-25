import aiohttp
import aiohttp.web


async def index_handler(request):
    return aiohttp.web.FileResponse("./index.html")


async def send_msgs(ws):
    for i in range(1000):
        await ws.send_str(f"Allo {i}!")


async def ws_handler(request):
    print(request)
    ws = aiohttp.web.WebSocketResponse()
    await ws.prepare(request)

    async for msg in ws:  # TODO: what is `async for` ?
        print(msg)

        match msg.type:
            case aiohttp.WSMsgType.TEXT:
                if msg.data == "close":
                    await ws.close()
                else:
                    await send_msgs(ws)
            case aiohttp.WSMsgType.ERROR:
                print(f"Closed with exception {ws.exception()}")

    print("Connection closed.")
    return ws


def init_func(argv):
    app = aiohttp.web.Application()
    app.add_routes(
        [aiohttp.web.get("/", index_handler), aiohttp.web.get("/myws", ws_handler)]
    )
    return app
