// Collapsible Examples - Auto-initialize on page load
(function () {
  "use strict";

  // Wait for DOM to be ready
  function initCollapsibleExamples() {
    const examples = document.querySelectorAll(".example");

    examples.forEach((example, index) => {
      // Wrap existing content in a collapsible container
      const content = Array.from(example.childNodes);
      const contentWrapper = document.createElement("div");
      contentWrapper.className = "example-content";
      contentWrapper.style.maxHeight = "0";
      contentWrapper.style.overflow = "hidden";
      contentWrapper.style.transition = "max-height 0.3s ease";

      // Move all content into wrapper (except title if it exists)
      const title = example.querySelector(".example-title");
      content.forEach((node) => {
        if (node !== title) {
          contentWrapper.appendChild(node);
        }
      });

      example.appendChild(contentWrapper);

      // Create toggle button
      const toggleBtn = document.createElement("button");
      toggleBtn.className = "example-toggle";
      toggleBtn.innerHTML = "▶";
      toggleBtn.setAttribute("aria-label", "Toggle example");
      toggleBtn.setAttribute("aria-expanded", "false");
      toggleBtn.style.cssText = `
                position: absolute;
                top: -0.6em;
                right: 1em;
                background: #ef4444;
                color: white;
                border: none;
                border-radius: 3px;
                width: 1.8em;
                height: 1.8em;
                cursor: pointer;
                font-size: 0.9em;
                display: flex;
                align-items: center;
                justify-content: center;
                transition: transform 0.3s ease;
                z-index: 10;
            `;

      // Insert button after title or at start
      if (title) {
        title.parentNode.insertBefore(toggleBtn, title.nextSibling);
      } else {
        example.insertBefore(toggleBtn, example.firstChild);
      }

      // Toggle function
      function toggleExample() {
        const isExpanded = toggleBtn.getAttribute("aria-expanded") === "true";

        if (isExpanded) {
          // Collapse
          contentWrapper.style.maxHeight = "0";
          toggleBtn.innerHTML = "▶";
          toggleBtn.setAttribute("aria-expanded", "false");
          toggleBtn.style.transform = "rotate(0deg)";
        } else {
          // Expand
          contentWrapper.style.maxHeight = contentWrapper.scrollHeight + "px";
          toggleBtn.innerHTML = "▼";
          toggleBtn.setAttribute("aria-expanded", "true");
          toggleBtn.style.transform = "rotate(0deg)";
        }
      }

      // Event listeners
      toggleBtn.addEventListener("click", toggleExample);

      // Keyboard support
      toggleBtn.addEventListener("keydown", (e) => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          toggleExample();
        }
      });

      // Handle window resize to recalculate max-height
      let resizeTimer;
      window.addEventListener("resize", () => {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(() => {
          if (toggleBtn.getAttribute("aria-expanded") === "true") {
            contentWrapper.style.maxHeight = contentWrapper.scrollHeight + "px";
          }
        }, 250);
      });
    });
  }

  // Initialize when DOM is ready
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initCollapsibleExamples);
  } else {
    initCollapsibleExamples();
  }
})();
