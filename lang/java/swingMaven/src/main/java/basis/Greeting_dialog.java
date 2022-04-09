package basis;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/*
Restrictions:

Using the ActionListener can only provide 1 action in this class.
TODO: define multiple actions

*/

public class Greeting_dialog
        extends JFrame
        implements ActionListener {

    private JLabel labelName;
    private JTextField fieldName;
    private JButton buttonGreeting;
    private GroupLayout layout;

    public Greeting_dialog() {

        initUI();
        initComponents();
    }

    public static void main(String[] args) {
        EventQueue.invokeLater(() -> {
            var ex = new Greeting_dialog();
            ex.setVisible(true);
        });
    }

    private void initUI() {
        setTitle("Greetings\uD83E\uDD73!");
        setLocationRelativeTo(null);
        setDefaultCloseOperation(EXIT_ON_CLOSE);

        setSize(500, 0);

    }

    private void initComponents() {
        labelName = new JLabel("Type your name");

        fieldName = new JTextField();
        fieldName.setColumns(10);

        buttonGreeting = new JButton("Great!");
        buttonGreeting.addActionListener(this);

        GroupLayout layout = new GroupLayout(getContentPane());
        getContentPane().setLayout(layout);

        layout.setAutoCreateGaps(true);
        layout.setAutoCreateContainerGaps(true);

        layout.setHorizontalGroup(
                layout.createSequentialGroup()
                        .addComponent(labelName)
                        .addComponent(fieldName)
                        .addComponent(buttonGreeting));
        layout.setVerticalGroup(
                layout.createParallelGroup()
                        .addComponent(labelName)
                        .addComponent(fieldName)
                        .addComponent(buttonGreeting));
        pack();
    }

    @Override
    public void actionPerformed(ActionEvent actionEvent) {
        String message = "Hello %s"; // Note this is a templated string.
        message = String.format(message, fieldName.getText());

        JOptionPane.showMessageDialog(this, message);
    }


}
