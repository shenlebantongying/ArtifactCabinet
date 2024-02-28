#include <adwaita.h>
#include <gtksourceview/gtksource.h>

static void
print_source_view_buffer_cb (GtkWidget *widget, gpointer data)
{
  GtkSourceView *view = data;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

  GtkTextIter start;
  GtkTextIter end;

  gtk_text_buffer_get_start_iter (buffer, &start);
  gtk_text_buffer_get_end_iter (buffer, &end);

  char *text = gtk_text_buffer_get_text (buffer, &start, &end, false);

  g_message ("%s", text);
}

static void
activate_cb (GtkApplication *app)
{

  GtkWidget *window = adw_application_window_new (app);

  GtkWidget *split_view = adw_overlay_split_view_new ();

  GtkWidget *sidebar = adw_toolbar_view_new ();
  GtkWidget *sidebar_headbar = adw_header_bar_new ();
  GtkWidget *sidebar_list = gtk_list_box_new ();

  GtkWidget *hello_btn = gtk_button_new_with_label ("Print text to stdout");

  GtkWidget *source_view = gtk_source_view_new ();
  GtkWidget *source_view_plus_scroll_bar = gtk_scrolled_window_new ();

  // sidebar
  gtk_list_box_append (GTK_LIST_BOX (sidebar_list), hello_btn);
  adw_toolbar_view_add_top_bar (ADW_TOOLBAR_VIEW (sidebar), sidebar_headbar);
  adw_toolbar_view_set_content (ADW_TOOLBAR_VIEW (sidebar), sidebar_list);

  adw_header_bar_set_decoration_layout (ADW_HEADER_BAR (sidebar_headbar),
                                        "close");

  // content

  gtk_scrolled_window_set_child (
      GTK_SCROLLED_WINDOW (source_view_plus_scroll_bar), source_view);

  adw_overlay_split_view_set_content (ADW_OVERLAY_SPLIT_VIEW (split_view),
                                      source_view_plus_scroll_bar);
  adw_overlay_split_view_set_sidebar (ADW_OVERLAY_SPLIT_VIEW (split_view),
                                      sidebar);

  g_signal_connect (hello_btn, "clicked",
                    G_CALLBACK (print_source_view_buffer_cb), source_view);

  adw_application_window_set_content (ADW_APPLICATION_WINDOW (window),
                                      split_view);
  gtk_window_set_default_size (GTK_WINDOW (window), 800, 600);
  gtk_window_present (GTK_WINDOW (window));
}

int
main (int argc, char *argv[])
{
  g_autoptr (AdwApplication) app = NULL;

  app = adw_application_new ("org.slbtty.Hello", G_APPLICATION_DEFAULT_FLAGS);

  gtk_source_init ();

  g_signal_connect (app, "activate", G_CALLBACK (activate_cb), NULL);

  return g_application_run (G_APPLICATION (app), argc, argv);
}
