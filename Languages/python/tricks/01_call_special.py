class O:
    v = 0

    def __call__(self, *args, **kwds):
        if len(args) == 0:
            return self.v
        elif len(args) == 1:
            self.v = args[0]
        else:
            print("nope")


a = O()

a(123)

print(a())
