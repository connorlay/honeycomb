import java.util.List;
public class Example
{
  private List<Widget> widgets;
  private String id;
  public List<Widget> getWidgets ()
  {
    return this.widgets;
  }
  public String getId ()
  {
    return this.id;
  }
  public void setWidgets (List<Widget> widgets)
  {
    this.widgets = widgets;
  }
  public void setId (String id)
  {
    this.id = id;
  }
  public static class Widget
  {
    private Component component;
    private String id;
    public Component getComponent ()
    {
      return this.component;
    }
    public String getId ()
    {
      return this.id;
    }
    public void setComponent (Component component)
    {
      this.component = component;
    }
    public void setId (String id)
    {
      this.id = id;
    }
    public static class Component
    {
      private String name;
      public String getName ()
      {
        return this.name;
      }
      public void setName (String name)
      {
        this.name = name;
      }
    }
  }
}