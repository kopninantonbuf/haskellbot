<p><strong>&laquo;������� �� �������� Haskell (��������-���)&raquo;</strong></p>
<p>&nbsp;</p>
<p><strong>���������� ������</strong></p>
<p>&nbsp;</p>
<p>������� ������� ������� �������� ���������� ���� ��� ����������� Telegram, ������� ����� ������������� ��������� �����������:</p>
<p>&nbsp;</p>
<ol>
<li>����� � ����������� ����������� ����� Haskell ������� ��� ��������� ������� � ������� ���������� ������� �� ����� Hoogle (<a href="https://www.haskell.org/hoogle/">https://www.haskell.org/hoogle/</a>).</li>
<li>����� ���������� ���������� � ������ ��������� ��� ������ ������� /help.</li>
<li>���������� ��������� �������, ����� ������� ��� ��������� ������� �� ������� �� ����� Hoogle ��� ����� ����� ������������ ������.</li>
</ol>
<p>&nbsp;</p>
<p><strong>��������� ������������</strong></p>
<p>&nbsp;</p>
<ol>
<li>������������ Telegram ��� �������������� � ����� ������ ����� ����� ���� � ���������� ����������� � ������ � ��� ������ �������� /start.</li>
<li>����� ������ �����, ����� ������ ��������� ������. ������ �������: /find &lt;��� �������&gt; || &lt;��������� �������&gt;. ��������, &laquo;/find min&raquo; ��� &laquo;/find (Integral a) =&gt; a -&gt; a&raquo;.</li>
<li>� ����� ������������ ����� �������� ������ ���� �������, ��������������� ������� � �������:</li>
</ol>
<p><em>&nbsp;</em></p>
<ol>
<li><em> &lt;�������� � ��������� ������� 1&gt;</em></li>
</ol>
<p><em>&lt;������������ ������� 1 � ����������&gt;</em></p>
<p><em>&lt;�������� ������� 1 � ��������� �������&gt;</em></p>
<p><em>&nbsp;</em></p>
<ol start="2">
<li><em> &lt;�������� � ��������� ������� 2&gt;</em></li>
</ol>
<p><em>&lt;������������ ������� 2 � ����������&gt;</em></p>
<p><em>&lt;�������� ������� 2 � ��������� �������&gt;</em></p>
<p>&nbsp;</p>
<p><em>&hellip;</em></p>
<p>&nbsp;</p>
<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ��������, �� ������ &laquo;/find minimumBy&raquo; � �������� ��������� ����� ������� ����� ���������.</p>
<p>&nbsp;</p>
<ol>
<li><em> minimumBy :: (a -&gt; a -&gt; Ordering) -&gt; [a] -&gt; a</em></li>
</ol>
<p><em>base Data.List</em></p>
<p><em>The minimumBy function takes a comparison function and a list and returns the least element of the list by the comparison function. The list must be finite and non-empty.</em></p>
<p><em>&nbsp;</em></p>
<ol start="2">
<li><em> minimumBy :: Foldable t =&gt; (a -&gt; a -&gt; Ordering) -&gt; t a -&gt; a</em></li>
</ol>
<p><em>base Data.Foldable</em></p>
<p><em>The least element of a non-empty structure with respect to the given comparison function.</em></p>
<p><em>&nbsp;</em></p>
<ol start="4">
<li>���� ������� �� �������, ����� ������� �����: &laquo;Function not found. Try again&raquo;.</li>
<li>��� ������������ �������, ����� ������� ����� &laquo;Incorrect request. Try again&raquo;.</li>
</ol>
<p><strong>&nbsp;</strong></p>
<p><strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ���������� ������ �������</strong></p>
<p>&nbsp;</p>
<ol>
<li>����� ����� ������� ������ ������������� �������� /find � ��� ��������� ��� ����� �������� �������� ������� � ������� String.</li>
<li>���� �������� ����� ������������ � ��������� ����� �� ����� Hoogle. � ����� ������������� �����.</li>
<li>�������� ����������� ������ ����� ��������������, � ��������� ������ ������� ����� ������� � ����� ������������ ����.</li>
</ol>
<p>&nbsp;</p>
