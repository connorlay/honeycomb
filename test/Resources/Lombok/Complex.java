import java.util.List;
import lombok.Data;

@Data
public class Complex {
  public String id;
  public List<Widget> widgets;

  @Data
  public static class Widget {
    public String id;
  }
}

