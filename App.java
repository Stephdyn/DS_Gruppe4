import java.util.Scanner;

public class App {
    public static void main(String[] args) {
        // Ausgabe "Hello World"
        System.out.println("Hello World");

        // Benutzer auffordern, etwas einzugeben
        try ( // Erstellen eines Scanner-Objekts für die Eingabe
                Scanner scanner = new Scanner(System.in)) {
            // Benutzer auffordern, etwas einzugeben
            System.out.print("Bitte gib etwas ein: ");
            // Lesen der Benutzereingabe als String
            String eingabe = scanner.nextLine();
            // Ausgabe der Benutzereingabe
            System.out.println("Du hast eingegeben: " + eingabe);
            // Schließen des Scanners
        }
    }

}
