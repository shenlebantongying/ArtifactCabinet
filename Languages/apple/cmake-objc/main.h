#import <Foundation/Foundation.h>

@interface Person : NSObject
    @property (assign, readwrite) NSString *name;
    @property (assign, readwrite) NSInteger id;
    - (void) printName;
    - (void) echoID;
@end
