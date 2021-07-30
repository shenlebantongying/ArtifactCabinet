package basis;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;

public class Mouse_tracker extends JFrame {

    private JLabel coords;

    public Mouse_tracker() {

        initUI();
    }

    private void initUI() {

        coords = new JLabel("");

        // This is a wrapper function to programmatically create layouts
        createLayout(coords);

        addMouseMotionListener(new MouseMotionAdapter() {

            @Override
            public void mouseMoved(MouseEvent e) {

                super.mouseMoved(e);

                int x = e.getX();
                int y = e.getY();

                var text = String.format("x: %d, y: %d", x, y);

                coords.setText(text);
            }
        });

        setTitle("Mouse move events");
        setSize(500, 500);
        setLocationRelativeTo(null);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
    }

    private void createLayout(JComponent... arg) {

        var pane = getContentPane();
        var layout = new GroupLayout(pane);
        pane.setLayout(layout);

        layout.setAutoCreateContainerGaps(true);

        layout.setHorizontalGroup(layout.createParallelGroup()
                .addComponent(arg[0])
        );

        layout.setVerticalGroup(layout.createSequentialGroup()
                .addComponent(arg[0])
        );

        pack();
    }


    public static void main(String[] args) {
        EventQueue.invokeLater(() -> {
            var ex = new Mouse_tracker();
            ex.setVisible(true);
        });
    }
}
