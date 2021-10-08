//
//  main.m
//  workbench-objc
//
//  Created by slbtty on 2021-10-08.
//
//

#import <Foundation/Foundation.h>

@interface Person : NSObject
    @property (readwrite) NSString *Name;
    @property (readwrite) NSInteger id;
    - (void) printName;
    - (void) echoID;
@end

@implementation Person
    -(void)printName {
        NSLog(@"This is: %@\n",self.Name);
    }

    -(void) echoID{
        NSLog(@"Id: %li\n",self.id);
    }

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {

        Person *John = [[Person alloc] init];
        John.Name=@"john";
        John.id = 123456;

        [John printName];
        [John echoID];

        NSLog(@"Hello, World!");
    }
    return 0;

}

