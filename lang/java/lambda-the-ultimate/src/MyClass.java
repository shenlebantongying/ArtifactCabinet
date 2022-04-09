public class MyClass {

    public abstract static class Event{
        public String name;
        public Event(String name) {
            this.name = name;
        };
        public abstract void execute();
        public final void finallize(){
            System.out.println("Even finished!");
        };
    }

    public void trigger(Event event) {
        event.execute();
        event.finallize();
    }

    public static void main(String... args) {
        MyClass Greeter = new MyClass();

        Greeter.trigger(
                new Event("Bonjure!") {
                    @Override
                    public void execute() {
                        System.out.println("Customized Event, " + this.name);
                    }
                }
        );
    }
}
