#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>

GtkWidget *window;
GtkWidget *label;
char *label_text;

void
label_append(const char *str)
{
    asprintf(&label_text,"%s %s",label_text,str);
    gtk_label_set_text(GTK_LABEL(label), label_text);
}


gboolean
key_press(GtkEventController *controller,
          guint keyval,
          guint keycode,
          GdkModifierType state)
{
    label_append(gdk_keyval_name(keyval));
    return TRUE;
}

static void
activate(GtkApplication *app,
         gpointer user_data)
{

    label_text=strdup("Key Event -> ");

    label = gtk_label_new(label_text);
        gtk_label_set_wrap(GTK_LABEL(label), TRUE);
        gtk_label_set_wrap_mode(GTK_LABEL(label), 1);
        gtk_label_set_xalign(GTK_LABEL(label), 0);
        gtk_label_set_yalign(GTK_LABEL(label), 0);

    window = gtk_application_window_new(app);
        gtk_window_set_title(GTK_WINDOW (window), "Window");
        gtk_window_set_default_size(GTK_WINDOW (window), 400, 400);
        gtk_window_set_child(GTK_WINDOW(window), label);


    GtkEventController *controller;
        controller = gtk_event_controller_key_new();
        g_signal_connect (controller, "key-pressed", G_CALLBACK(key_press), NULL);
        gtk_widget_add_controller(GTK_WIDGET(window), controller);

    gtk_widget_show(label);
    gtk_widget_show(window);
}

int
main(int argc, char **argv)
{
    GtkApplication *app;
    int status;

    app = gtk_application_new("org.gtk.example", G_APPLICATION_FLAGS_NONE);
    g_signal_connect (app, "activate", G_CALLBACK(activate), NULL);

    status = g_application_run(G_APPLICATION (app), argc, argv);
    g_object_unref(app);

    return status;
}