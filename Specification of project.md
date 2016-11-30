<p><strong>&laquo;Справка по функциям Haskell (телеграм-бот)&raquo;</strong></p>
<p>&nbsp;</p>
<p><strong>Постановка задачи</strong></p>
<p>&nbsp;</p>
<p>Задачей данного проекта является реализация бота для мессенджера Telegram, который будет реализовывать следующие возможности:</p>
<p>&nbsp;</p>
<ol>
<li>Поиск в стандартных библиотеках языка Haskell функции или сигнатуры функции с помощью поискового запроса на сайте Hoogle (<a href="https://www.haskell.org/hoogle/">https://www.haskell.org/hoogle/</a>).</li>
<li>Показ справочной информации о работе программы при вызове команды /help.</li>
<li>Корректная обработка случаев, когда функция или сигнатура функции не найдена на сайте Hoogle или когда введён некорректный запрос.</li>
</ol>
<p>&nbsp;</p>
<p><strong>Интерфейс пользователя</strong></p>
<p>&nbsp;</p>
<ol>
<li>Пользователь Telegram для взаимодействия с ботом должен будет найти бота в поисковике мессенджера и начать с ним диалог командой /start.</li>
<li>Когда диалог начат, можно делать поисковый запрос. Формат запроса: /find &lt;имя функции&gt; || &lt;сигнатура функции&gt;. Например, &laquo;/find min&raquo; или &laquo;/find (Integral a) =&gt; a -&gt; a&raquo;.</li>
<li>В ответ пользователь будет получать список всех функций, удовлетворяющих запросу в формате:</li>
</ol>
<p><em>&nbsp;</em></p>
<ol>
<li><em> &lt;название и сигнатура функции 1&gt;</em></li>
</ol>
<p><em>&lt;расположение функции 1 в библиотеке&gt;</em></p>
<p><em>&lt;описание функции 1 в текстовом формате&gt;</em></p>
<p><em>&nbsp;</em></p>
<ol start="2">
<li><em> &lt;название и сигнатура функции 2&gt;</em></li>
</ol>
<p><em>&lt;расположение функции 2 в библиотеке&gt;</em></p>
<p><em>&lt;описание функции 2 в текстовом формате&gt;</em></p>
<p>&nbsp;</p>
<p><em>&hellip;</em></p>
<p>&nbsp;</p>
<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Например, на запрос &laquo;/find minimumBy&raquo; в ответном сообщении будет получен такой результат.</p>
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
<li>Если функция не найдена, будет получен ответ: &laquo;Function not found. Try again&raquo;.</li>
<li>При некорректном запросе, будет получен ответ &laquo;Incorrect request. Try again&raquo;.</li>
</ol>
<p><strong>&nbsp;</strong></p>
<p><strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Реализация поиска функции</strong></p>
<p>&nbsp;</p>
<ol>
<li>После ввода запроса поиска пользователем командой /find и его обработки бот будет получать параметр запроса в формате String.</li>
<li>Этот параметр будет передаваться в поисковую форму на сайте Hoogle. И будет производиться поиск.</li>
<li>Страница результатов поиска будет распарсиваться, а результат работы парсера будет передан в ответ пользователю бота.</li>
</ol>
<p>&nbsp;</p>
