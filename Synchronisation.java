

public class Synchronisation {
    public static void main(String[] args) {
        // Counter Instanzen anlegen
        Counter counter1 = new Counter();
       // Counter counter2 = new Counter();

        
        Thread thread1 = new Thread(() -> {
            for (int i = 0; i < 1000; i++) {

                int expected = counter1.value() + 1;
                counter1.increment(); 
                int value = counter1.value();
                System.out.println(value + " Inkrement " + expected + " Expected"); 
                if (value != expected) {
                    throw new RuntimeException("Inkrementierung: " + value + " - " + expected);
                }              
            }
        });

        Thread thread2 = new Thread(() -> {
            for (int i = 0; i < 1000; i++) {
                int expected = counter1.value() - 1;
                counter1.decrement();
                int value = counter1.value();
                System.out.println(value + " Dekrement " + expected + " Expected"); 
                if (value != expected) {
                    throw new RuntimeException("Dekrementierung: " + value + " - " + expected);
                }   
            }
        });

        thread1.start();
        thread2.start();

        // Warten bis beide Threads fertig sind
        try {
            thread1.join();
            thread2.join();
        } catch (InterruptedException e) {
            System.out.println("Thread interrupted: " + e.getMessage());
        }


        System.out.println("Final value of counter1: " + counter1.value());
        //System.out.println("Final value of counter2: " + counter2.value());
    }
}

