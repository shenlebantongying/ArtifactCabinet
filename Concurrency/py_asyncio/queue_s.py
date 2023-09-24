import asyncio


# asyncio.Queue is a data container

async def worker(name, Q):
    while True:
        item_time = await Q.get()
        await asyncio.sleep(item_time/5)
        print(f'{name} -> {item_time}')
        Q.task_done()


async def main():
    Q = asyncio.Queue()

    for i in range(10):
        Q.put_nowait(i)

    # worker/executor
    executors = []
    for i in range(3):
        t = asyncio.create_task(worker(f'Worker-{i}', Q))
        executors.append(t)

    await Q.join()

    print("Before Cancel!")

    for i in executors:
        i.cancel()

    await asyncio.gather(*executors, return_exceptions=True)

    print("After gather")


asyncio.run(main())

'''
Sample output:

Worker-0 -> 0
Worker-1 -> 1
Worker-2 -> 2
Worker-0 -> 3
Worker-1 -> 4
Worker-2 -> 5
Worker-0 -> 6
Worker-1 -> 7
Worker-2 -> 8
Worker-0 -> 9
Before Cancel!
After gather
'''