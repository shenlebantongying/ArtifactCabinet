use futures::executor::block_on; // sadly, it is a crate

async fn holy(count: i32) {
    for i in 0..=count {
        println!("C is {i}");
    }
}

async fn asy_m(count:i32){
    holy(count).await;
}

fn main(){
 block_on(asy_m(100));
}