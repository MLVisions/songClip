// JavaScript function to update the red line position
function updateRedLinePosition(plotlyElement, seekValue) {
  // Find the red line shape by its name or index
  var shapeIndex = null; // Set this to the index of your red line shape
  var shapes = plotlyElement.layout.shapes;

  // Update the x0 and x1 attributes of the red line shape
  if (shapeIndex !== null && shapes[shapeIndex]) {
    shapes[shapeIndex].x0 = seekValue;
    shapes[shapeIndex].x1 = seekValue;

    // Update the Plotly chart
    Plotly.update(plotlyElement);
  }
}

// Example usage: call this function with the seek value
// var plotlyElement = document.getElementById('your-plotly-element-id'); // Replace with your element ID
// var seekValue = 30; // Replace with the current seek value
// updateRedLinePosition(plotlyElement, seekValue);
