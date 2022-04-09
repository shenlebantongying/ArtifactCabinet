#import "main.h"

@implementation Person
-(void)printName {
    NSLog(@"This is: %@\n",self.name);
}

-(void) echoID{
    NSLog(@"Id: %li\n",self.id);
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {

        Person *John = [[Person alloc] init];
        John.name=@"john";
        John.id = 123456;

        [John printName];
        [John echoID];

        NSLog(@"Hello, World!");
    }
    return 0;

}
