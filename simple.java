import java.util.List;

public class GetANoteResponse {

  private String id;
  private String title;
  private String content;
  private List<String> tags;

  public String getId() {
    return id;
  }

  public String getTitle() {
    return title;
  }

  public String getContent() {
    return content;
  }

  public List<String> getTags() {
    return tags;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  public void setContent(String content) {
    this.content = content;
  }

  public void setTags(List<String> tags) {
    this.tags = tags;
  }
}
