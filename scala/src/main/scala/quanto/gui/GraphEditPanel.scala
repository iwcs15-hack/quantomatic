package quanto.gui

import graphview.GraphView
import quanto.data._
import swing._
import swing.event._
import javax.swing.ImageIcon
import quanto.util.swing.ToolBar
import quanto.util.SockJson

class GraphEditPanel(theory: Theory, val readOnly: Boolean = false) extends BorderPanel {
  // GUI components
  val graphView = new GraphView(theory) {
    drawGrid = true
    dynamicResize = true
    focusable = true
  }

  val graphDocument = new GraphDocument(graphView)
  def graph = graphDocument.graph
  def graph_=(g: Graph) { graphDocument.graph = g }

  // alias for graph_=, used in java code
  def setGraph(g: Graph) { graph_=(g) }


  val VertexTypeLabel  = new Label("Vertex Type:  ") { xAlignment = Alignment.Right; enabled = false }
  val VertexTypeSelect = new ComboBox("<wire>" +: theory.vertexTypes.keys.toSeq) { enabled = false }
  val EdgeTypeLabel    = new Label("Edge Type:  ") { xAlignment = Alignment.Right; enabled = false }
  val EdgeTypeSelect   = new ComboBox(theory.edgeTypes.keys.toSeq) { enabled = false }
  val EdgeDirected     = new CheckBox("directed") { selected = true; enabled = false }

  // Bottom panel
  object BottomPanel extends GridPanel(1,5) {
    contents += (VertexTypeLabel, VertexTypeSelect)
    contents += (EdgeTypeLabel, EdgeTypeSelect, EdgeDirected)
  }

  val graphEditController = new GraphEditController(graphView, readOnly) {
    undoStack            = graphDocument.undoStack
    vertexTypeSelect     = VertexTypeSelect
    edgeTypeSelect       = EdgeTypeSelect
    edgeDirectedCheckBox = EdgeDirected
  }

  val GraphViewScrollPane = new ScrollPane(graphView)

  trait ToolButton { var tool: MouseState = SelectTool() }

  val SelectButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("select-rectangular.png"), "Select")
    selected = true
    tool = SelectTool()
  }

  val AddVertexButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("draw-ellipse.png"), "Add Vertex")
    tool = AddVertexTool()
  }

  val AddEdgeButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("draw-path.png"), "Add Edge")
    tool = AddEdgeTool()
  }

  val AddBangBoxButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("draw-bang.png"), "Add Bang Box")
    tool = AddBangBoxTool()
  }

  val ReLayoutButton = new Button("Re-Layout")
  val ConnectButton = new Button("Connect")
  val PrevButton = new Button("Prev")
  val NextButton = new Button("Next")
  val DisconnectButton = new Button("Disconnect")

   def setEvalButtonStatus (con : Boolean, discon : Boolean, prev : Boolean, next : Boolean) {
     PrevButton.enabled_=(prev);
     NextButton.enabled_=(next);
     ConnectButton.enabled_=(con);
     DisconnectButton.enabled_=(discon);

   }

  val GraphToolGroup = new ButtonGroup(SelectButton,
                                       AddVertexButton, 
                                       AddEdgeButton,
                                       AddBangBoxButton,
                                       ReLayoutButton
                                      )

  val MainToolBar = new ToolBar {
    contents += (SelectButton, AddVertexButton, AddEdgeButton, AddBangBoxButton);
    addSeparator();
    contents += (ReLayoutButton)
    addSeparator();
    contents += (ConnectButton, DisconnectButton, PrevButton, NextButton)
  }


  if (!readOnly) {
    add(MainToolBar, BorderPanel.Position.North)
    add(BottomPanel, BorderPanel.Position.South)
  }

  add(GraphViewScrollPane, BorderPanel.Position.Center)


  listenTo(GraphViewScrollPane, graphDocument)
  GraphToolGroup.buttons.foreach(listenTo(_))
  reactions += {
    case UIElementResized(GraphViewScrollPane) => graphView.repaint()
    case ButtonClicked(t: ToolButton) =>
      graphEditController.mouseState = t.tool
      t.tool match {
        case SelectTool() =>
          VertexTypeLabel.enabled = false
          VertexTypeSelect.enabled = false
          EdgeTypeLabel.enabled = false
          EdgeTypeSelect.enabled = false
          EdgeDirected.enabled = false
        case AddVertexTool() =>
          VertexTypeLabel.enabled = true
          VertexTypeSelect.enabled = true
          EdgeTypeLabel.enabled = false
          EdgeTypeSelect.enabled = false
          EdgeDirected.enabled = false
        case AddEdgeTool() =>
          VertexTypeLabel.enabled = false
          VertexTypeSelect.enabled = false
          EdgeTypeLabel.enabled = true
          EdgeTypeSelect.enabled = true
          EdgeDirected.enabled = true
        case AddBangBoxTool() =>
          VertexTypeLabel.enabled = false
          VertexTypeSelect.enabled = false
          EdgeTypeLabel.enabled = false
          EdgeTypeSelect.enabled = false
          EdgeDirected.enabled = false
        case _ =>
      }
    case ButtonClicked (ReLayoutButton) =>
      graphDocument.reLayout();
    case ButtonClicked (ConnectButton) =>
      SockJson.connect_sock();
      val edata = SockJson.request_init();
      graphDocument.loadGraph (edata);
      graphDocument.reLayout();
      setEvalButtonStatus (false, true, false, true)
    case ButtonClicked (DisconnectButton) =>
      SockJson.request_deinit ();
      SockJson.close_sock ();
      setEvalButtonStatus (true, false, false, false);

  }
}
