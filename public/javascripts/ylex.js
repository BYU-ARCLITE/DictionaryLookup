var YLex = (function(){

	function getReprRenderer(name){
		switch(name){
		case "WAV": return function(value){
			return '<audio controls><source src="'
					+ value + '" type="audio/wav"></audio>';
		}
		default: return function(value){ return value; }
		}
	}

	function processRepr(name, values){
		var renderer = getReprRenderer(name);
		return values.map(renderer).join(", ");
	}

	function renderSense(sense){
		var html = sense.definition;
		if(sense.examples && sense.examples.length){
			html += "<br/><i>Examples:</i>"
				 + sense.examples.map(function(s){ return "<br/>"+s; });
		}
		if(sense.notes && sense.notes.length){
			html += "<br/><i>Notes:</i>"
				 + sense.notes.map(function(s){ return "<br/>"+s; });
		}
		return html;
	}

	function renderLemma(lemma){
		var html, prefRep = lemma.representations[0],
			lemmaForm = lemma.forms[lemma.lemmaForm],
			repList = Object.keys(lemmaForm),
			formList = Object.keys(lemma.forms);

		html = "<b>"+processRepr(prefRep, lemmaForm[prefRep])+"</b>";

		if(repList.length > 1){
			html += "<dl>";
			repList.forEach(function(repr){
				if(repr === prefRep){ return; }
				html += "<dt>"+repr+"</dt><dd>"
					 +processRepr(repr, lemmaForm[repr]) + "</dd>";
			});
			html += "</dl>";
		}

		if(lemma.pos){
			html += "<div>"+lemma.pos+"</div>";
		}

		if(formList.length > 1){
			html += "<div><i>Other Forms:</i><dl>";
			formList.forEach(function(fname){
				if(fname == lemma.lemmaForm){ return; }
				var form = lemma.forms[fname];
				html += "<dt>"+fname+"</dt><dd>";
				if(repList.length > 1){
					html += "<dl>" + Object.keys(form).map(function(repr){
						return "<dt>"+repr+"</dt><dd>"
							+ processRepr(repr, form[repr]) + "</dd>";
					}).join('')+"</dl>";
				}else{
					html += processRepr(repList[0], form[repList[0]]);
				}
				html += "</dd>";
			});
			html += "</dl></div>";
		}

		if(lemma.senses && lemma.senses.length){
			html += "<div><ol><li>"
				 + lemma.senses.map(renderSense).join("</li><li>")
				 + "</li></ol></div>";
		}

		html += lemma.sources.map(function(source){
			return '<div class="source">'+source.attribution+'</div>';
		}).join("<br/>");

        return html;
	}

	function renderWord(text, word){
		var original = text.substring(word.start, word.end);
		return "<b>"+original+"</b><ol><li>"
				+ word.lemmas.map(renderLemma).join("</li><li>")
				+ "</li></ol>";
	}

	function renderResult(result){
		var html = '<div class="sourceText"><b>Original Text:</b>&nbsp;' + result.text + '</div>';
		if(result.translations){
			html += '<div class="translationResult">\
						<b>Free Translations:</b>\
						<div class="translations">' +
							result.translations.map(function(trans){
								return '"'+trans.text
										  + '"<div class="source">'
										  + trans.source.attribution
										  + '</div>'
							}).join("") +
						'</div>\
					</div>';
		}
		if(result.words){
			html += '<div class="translationResult">\
						<b>Definitions:</b>\
						<div class="translations">' +
							result.words.map(function(word){
								return renderWord(result.text, word);
							}).join("") +
						'</div>\
					</div>';
		}
		return html;
	}

	return {
		"renderResult": renderResult
	};
}());