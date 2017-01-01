import java.util.List;

public class Complex {

  public String id;
  public List<Widget> widgets;

  public static class Widget {

    public String id;

    public String getId() {
      return id;
    }

    public void setId(String id) {
      this.id = id;
    }
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public List<Widget> getWidgets() {
    return widgets;
  }

  public void setWidgets(List<Widget> widgets) {
    this.widgets = widgets;
  }
}
